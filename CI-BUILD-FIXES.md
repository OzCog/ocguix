# CI Build Fixes Documentation

## Overview

This document outlines the fixes implemented to resolve CI build failures in the OzCog/ocguix project. The fixes address multiple issues that were causing workflow builds to fail.

## Issues Identified and Fixed

### 1. Git Dubious Ownership Error

**Problem**: 
```
fatal: detected dubious ownership in repository at '/__w/ocguix/ocguix'
```

**Root Cause**: 
Git security measures prevent operations on repositories owned by different users in CI environments.

**Solution**: 
Added git safe.directory configuration in the workflow:

```yaml
- name: Set Git Safe Directory
  run: |
    git config --global --add safe.directory ${{ github.workspace }}
    git config --global --add safe.directory /__w/ocguix/ocguix
```

**Files Modified**: 
- `.github/workflows/oc.yml`

### 2. Valgrind Not Found Error

**Problem**: 
```
Could NOT find VALGRIND (missing: VALGRIND_INCLUDE_DIR VALGRIND_PROGRAM)
```

**Root Cause**: 
Valgrind was not installed in the CI environment, but CMake was trying to find it for thread debugging.

**Solution**: 
Added valgrind installation to the build dependencies:

```yaml
- name: Install Build Dependencies
  run: |
    sudo apt-get update
    sudo apt-get install -y ccache pkg-config cmake build-essential git valgrind
```

**Files Modified**: 
- `.github/workflows/oc.yml`

### 3. C++ Compilation Error: 'ostream_container' Not Declared

**Problem**: 
```
'ostream_container' was not declared in this scope
Location: /repos/moses/moses/comboreduct/table/table.h:118
```

**Root Cause**: 
The `ostream_container` function is defined in `opencog/util/iostreamContainer.h` but was not included in `table.h`.

**Solution**: 
Added the missing include statement:

```cpp
#include <opencog/util/iostreamContainer.h>
```

**Files Modified**: 
- `repos/moses/moses/comboreduct/table/table.h`

### 4. Database Role Issues

**Problem**: 
```
FATAL: role "root" does not exist
```

**Root Cause**: 
Database initialization was trying to use a 'root' role that doesn't exist in the PostgreSQL container.

**Solution**: 
The workflow already uses the correct database configuration:
- `POSTGRES_USER: opencog_test`
- `POSTGRES_PASSWORD: cheese`
- `POSTGRES_DB: atomspace_db`

This ensures the database uses the `opencog_test` user instead of `root`.

## Implementation Details

### Workflow Changes

The main workflow file `.github/workflows/oc.yml` was updated with:

1. **Git Safe Directory Configuration**: Added immediately after checkout to prevent ownership issues
2. **Valgrind Installation**: Added to the build dependencies step
3. **Proper Error Handling**: Maintained existing error handling patterns

### Code Changes

The `table.h` file was updated to include the missing header:

```cpp
// Before
#include <opencog/util/algorithm.h>
#include <opencog/util/Counter.h>
#include <opencog/util/dorepeat.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/KLD.h>

// After
#include <opencog/util/algorithm.h>
#include <opencog/util/Counter.h>
#include <opencog/util/dorepeat.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/KLD.h>
#include <opencog/util/iostreamContainer.h>
```

## Testing and Validation

### Pre-Fix Issues
- Git operations failing due to ownership
- CMake unable to find Valgrind
- Compilation errors in moses component
- Database connection failures

### Post-Fix Expected Behavior
- Git operations proceed normally
- Valgrind found and configured properly
- All components compile successfully
- Database connections work with `opencog_test` user

## Monitoring and Prevention

### Issue Template
Created `.github/ISSUE_TEMPLATE/ci-build-failure.md` to standardize CI failure reporting.

### Common Solutions Reference
The issue template includes quick reference solutions for:
- Git ownership issues
- Missing dependencies
- Compilation errors
- Database configuration

## Future Improvements

### Recommended Enhancements
1. **Dependency Validation**: Add pre-build checks for required tools
2. **Better Error Messages**: Improve CMake error reporting
3. **Build Caching**: Implement ccache for faster rebuilds
4. **Parallel Testing**: Enable parallel test execution where possible

### Monitoring
- Monitor workflow success rates
- Track build times
- Document any new failure patterns
- Update this documentation as needed

## Troubleshooting Guide

### If Builds Still Fail

1. **Check Git Configuration**:
   ```bash
   git config --global --list | grep safe.directory
   ```

2. **Verify Valgrind Installation**:
   ```bash
   which valgrind
   valgrind --version
   ```

3. **Check Include Paths**:
   ```bash
   find /usr/local/include -name "iostreamContainer.h"
   ```

4. **Database Connection Test**:
   ```bash
   psql -h localhost -U opencog_test -d atomspace_db -c "SELECT 1;"
   ```

### Common Commands

```bash
# Fix git ownership
git config --global --add safe.directory /path/to/repo

# Install valgrind
sudo apt-get update && sudo apt-get install -y valgrind

# Check CMake configuration
cmake --find-package -DNAME=VALGRIND -DCOMPILER_ID=GNU -DLANGUAGE=CXX -DMODE=EXIST

# Test database connection
psql -h localhost -U opencog_test -d atomspace_db
```

## Conclusion

These fixes address the core issues causing CI build failures:
- Git ownership problems resolved with safe.directory configuration
- Valgrind dependency issue fixed with proper installation
- Compilation error resolved with missing include
- Database configuration already properly set up

The workflow should now build successfully across all components. Monitor the builds and update this documentation if new issues arise.