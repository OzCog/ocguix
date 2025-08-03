---
name: CI Build Failure
about: Report a CI/CD workflow build failure
title: 'CI Build Failure: [Brief Description]'
labels: ['ci', 'build-failure', 'bug']
assignees: ''
---

## 🚨 CI Build Failure Report

### **Issue Type**
- [ ] Compilation Error
- [ ] Test Failure
- [ ] Dependency Issue
- [ ] Environment Problem
- [ ] Database Connection Issue
- [ ] Other

### **Workflow Affected**
- [ ] `oc.yml` - Main OpenCog build
- [ ] `guix.yml` - Guix integration
- [ ] `cognitive-ecosystem.yml` - Cognitive ecosystem
- [ ] `efficient-build.yml` - Efficient build
- [ ] Other: _______________

### **Error Details**

#### **Error Message**
```
[Paste the exact error message here]
```

#### **Build Log Location**
- **Workflow Run**: [Link to failed workflow run]
- **Job**: [Job name]
- **Step**: [Step name]
- **Timestamp**: [When the error occurred]

### **Root Cause Analysis**

#### **1. Git Ownership Issues**
- [ ] `fatal: detected dubious ownership in repository`
- **Solution**: Add git safe.directory configuration
- **Status**: [ ] Fixed [ ] Pending [ ] Not Applicable

#### **2. Missing Dependencies**
- [ ] Valgrind not found
- [ ] CMake package missing
- [ ] Compiler toolchain issue
- **Solution**: Install missing packages
- **Status**: [ ] Fixed [ ] Pending [ ] Not Applicable

#### **3. Compilation Errors**
- [ ] Missing include files
- [ ] Undefined symbols
- [ ] Template instantiation errors
- **Solution**: Add missing includes or fix code
- **Status**: [ ] Fixed [ ] Pending [ ] Not Applicable

#### **4. Database Issues**
- [ ] Role "root" does not exist
- [ ] Connection refused
- [ ] Authentication failed
- **Solution**: Fix database configuration
- **Status**: [ ] Fixed [ ] Pending [ ] Not Applicable

### **Steps to Reproduce**
1. [Step 1]
2. [Step 2]
3. [Step 3]

### **Expected Behavior**
[Describe what should happen]

### **Actual Behavior**
[Describe what actually happened]

### **Environment Information**
- **OS**: [Ubuntu/Windows/macOS]
- **Runner**: [GitHub-hosted/Self-hosted]
- **Container**: [Docker image used]
- **Git Version**: [Git version]
- **CMake Version**: [CMake version]

### **Proposed Solutions**

#### **Immediate Fixes**
- [ ] Add git safe.directory configuration
- [ ] Install missing dependencies (valgrind, etc.)
- [ ] Add missing include statements
- [ ] Fix database user/role configuration
- [ ] Update CMake configuration

#### **Long-term Improvements**
- [ ] Add dependency checking in workflow
- [ ] Implement better error handling
- [ ] Add build validation steps
- [ ] Improve documentation

### **Files Modified**
- [ ] `.github/workflows/[workflow-name].yml`
- [ ] `repos/[component]/[file]`
- [ ] `CMakeLists.txt`
- [ ] Other: _______________

### **Testing**
- [ ] Local build successful
- [ ] CI build successful
- [ ] Tests passing
- [ ] Documentation updated

### **Additional Notes**
[Any additional context, screenshots, or information]

---

## 🔧 Common Solutions

### **Git Ownership Fix**
```bash
git config --global --add safe.directory ${{ github.workspace }}
git config --global --add safe.directory /__w/ocguix/ocguix
```

### **Valgrind Installation**
```bash
sudo apt-get update && sudo apt-get install -y valgrind
```

### **Missing Include Fix**
```cpp
#include <opencog/util/iostreamContainer.h>
```

### **Database Role Fix**
```sql
CREATE ROLE opencog_test;
GRANT ALL PRIVILEGES ON DATABASE atomspace_db TO opencog_test;
```

---

**Labels**: `ci`, `build-failure`, `bug`
**Priority**: [High/Medium/Low]
**Estimated Effort**: [1-2 hours / 1 day / 1 week]