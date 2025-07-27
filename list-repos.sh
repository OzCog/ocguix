#!/bin/bash
# List repositories included in the monorepo
# Part of the monorepo integration for issue #128

echo "🧠 OpenCog/Guix Monorepo Repository Listing"
echo "==========================================="
echo ""

if [ ! -d "repos" ]; then
    echo "❌ repos/ directory not found!"
    exit 1
fi

cd repos

echo "📦 Repositories included in this monorepo:"
echo ""

for repo in */; do
    if [ -d "$repo" ]; then
        repo_name=$(basename "$repo")
        echo "📁 $repo_name/"
        
        # Check for README files
        if [ -f "$repo/README.md" ]; then
            echo "   📄 README.md found"
            # Extract first line of description if available
            first_line=$(head -n 5 "$repo/README.md" | grep -v "^#" | grep -v "^$" | head -n 1)
            if [ ! -z "$first_line" ]; then
                echo "   ℹ️  $(echo "$first_line" | cut -c1-70)..."
            fi
        elif [ -f "$repo/README" ]; then
            echo "   📄 README found"
        fi
        
        # Check for build files
        if [ -f "$repo/CMakeLists.txt" ]; then
            echo "   🔧 CMakeLists.txt (CMake project)"
        elif [ -f "$repo/Makefile" ]; then
            echo "   🔧 Makefile (Make project)"
        elif [ -f "$repo/setup.py" ]; then
            echo "   🐍 setup.py (Python project)"
        elif [ -f "$repo/pyproject.toml" ]; then
            echo "   🐍 pyproject.toml (Python project)"
        elif [ -f "$repo/package.json" ]; then
            echo "   📦 package.json (Node.js project)"
        fi
        
        # Count major file types
        c_files=$(find "$repo" -name "*.c" -o -name "*.cpp" -o -name "*.cc" -o -name "*.cxx" 2>/dev/null | wc -l)
        h_files=$(find "$repo" -name "*.h" -o -name "*.hpp" -o -name "*.hxx" 2>/dev/null | wc -l)
        py_files=$(find "$repo" -name "*.py" 2>/dev/null | wc -l)
        scm_files=$(find "$repo" -name "*.scm" 2>/dev/null | wc -l)
        
        if [ $c_files -gt 0 ] || [ $h_files -gt 0 ]; then
            echo "   💻 C/C++ files: $c_files source, $h_files headers"
        fi
        if [ $py_files -gt 0 ]; then
            echo "   🐍 Python files: $py_files"
        fi
        if [ $scm_files -gt 0 ]; then
            echo "   🧠 Scheme files: $scm_files"
        fi
        
        # Check if git history was removed
        if [ ! -d "$repo/.git" ]; then
            echo "   ✅ Git history removed (monorepo integration complete)"
        else
            echo "   ⚠️  Git history still present"
        fi
        
        echo ""
    fi
done

echo "🎯 Integration Status:"
echo "====================="
echo ""

total_repos=$(find . -maxdepth 1 -type d | wc -l)
total_repos=$((total_repos - 1))  # Exclude current directory
echo "   📊 Total repositories: $total_repos"

git_dirs=$(find . -name ".git" -type d | wc -l)
if [ $git_dirs -eq 0 ]; then
    echo "   ✅ All repositories properly integrated (no .git directories)"
else
    echo "   ⚠️  $git_dirs repositories still have .git directories"
fi

echo ""
echo "🛠️  To use these repositories:"
echo "   • Scripts now automatically use local copies"
echo "   • No more external cloning required"
echo "   • All source code is version controlled in this monorepo"
echo ""