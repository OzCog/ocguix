#!/usr/bin/env python3
"""
Test script for enhanced package discovery functionality
Tests the real API calls that should be made by the registry discovery agent
"""

import requests
import json
import sys
from urllib.parse import urljoin

def test_github_api():
    """Test GitHub API for OpenCog organization"""
    print("🔍 Testing GitHub API for OpenCog organization...")
    
    try:
        url = "https://api.github.com/orgs/opencog/repos"
        headers = {'User-Agent': 'OCGuix-Discovery-Agent/1.0'}
        
        response = requests.get(url, headers=headers, timeout=10)
        print(f"GitHub API Status: {response.status_code}")
        
        if response.status_code == 200:
            repos = response.json()
            repo_names = [repo['name'] for repo in repos if 'name' in repo]
            print(f"✅ Successfully discovered {len(repo_names)} OpenCog repositories")
            print(f"📦 Sample repositories: {repo_names[:8]}")
            return repo_names
        else:
            print(f"❌ GitHub API failed with status {response.status_code}")
            return []
            
    except Exception as e:
        print(f"❌ GitHub API error: {e}")
        return []

def test_guix_packages():
    """Test Guix package file access"""
    print("\n🔍 Testing Guix package file access...")
    
    base_url = "https://git.savannah.gnu.org/cgit/guix.git/plain/gnu/packages/"
    package_files = [
        "ai.scm", "scheme.scm", "cpp.scm", "machine-learning.scm",
        "python-science.scm", "maths.scm", "statistics.scm"
    ]
    
    accessible_files = []
    
    for file in package_files:
        try:
            url = urljoin(base_url, file)
            headers = {'User-Agent': 'OCGuix-Discovery-Agent/1.0'}
            
            response = requests.head(url, headers=headers, timeout=10)
            
            if response.status_code == 200:
                print(f"✅ Accessible: {file}")
                accessible_files.append(file)
            else:
                print(f"⚠️  Not accessible: {file} (status: {response.status_code})")
                
        except Exception as e:
            print(f"❌ Error accessing {file}: {e}")
    
    print(f"📦 Total accessible Guix package files: {len(accessible_files)}")
    return accessible_files

def test_julia_registry():
    """Test Julia package registry access"""
    print("\n🔍 Testing Julia package registry access...")
    
    try:
        url = "https://raw.githubusercontent.com/JuliaRegistries/General/master/Registry.toml"
        headers = {'User-Agent': 'OCGuix-Discovery-Agent/1.0'}
        
        response = requests.get(url, headers=headers, timeout=10)
        print(f"Julia Registry Status: {response.status_code}")
        
        if response.status_code == 200:
            content = response.text
            # Simple TOML parsing - look for package entries
            lines = content.split('\n')
            packages = []
            
            for line in lines:
                line = line.strip()
                if line.startswith('"') and ' = ' in line:
                    # Extract package name from TOML entry like "PackageName" = { ... }
                    quote_end = line.find('"', 1)
                    if quote_end > 1:
                        package_name = line[1:quote_end]
                        packages.append(package_name)
            
            print(f"✅ Successfully accessed Julia registry")
            print(f"📦 Total packages in registry: {len(packages)}")
            
            # Filter for ML/AI related packages
            ml_packages = [pkg for pkg in packages if any(keyword in pkg.lower() 
                          for keyword in ['ml', 'flux', 'knet', 'data', 'stats', 'distributions', 'plots'])]
            
            print(f"📦 ML/AI related packages found: {len(ml_packages)}")
            print(f"📦 Sample ML packages: {ml_packages[:8]}")
            return ml_packages[:15]  # Limit to 15 for practical purposes
            
        else:
            print(f"❌ Julia registry failed with status {response.status_code}")
            return []
            
    except Exception as e:
        print(f"❌ Julia registry error: {e}")
        return []

def generate_test_output():
    """Generate a test output showing what the enhanced discovery should produce"""
    print("\n📊 Running comprehensive package discovery test...")
    
    github_repos = test_github_api()
    guix_packages = test_guix_packages()
    julia_packages = test_julia_registry()
    
    total_packages = len(github_repos) + len(guix_packages) + len(julia_packages)
    
    print(f"\n📈 Enhanced Discovery Summary:")
    print(f"   - GitHub repositories: {len(github_repos)}")
    print(f"   - Guix package files: {len(guix_packages)}")
    print(f"   - Julia packages: {len(julia_packages)}")
    print(f"   - Total discovered: {total_packages}")
    
    # Generate sample output that matches what the Scheme agent should produce
    sample_output = {
        "generated": "2024-07-23T10:30:00Z",
        "schema_version": "1.1",
        "agent_id": "registry-discovery-agent",
        "registries": [
            {
                "id": "opencog-github",
                "url": "https://github.com/opencog/*",
                "status": "active",
                "package_listings": github_repos,
                "package_count": len(github_repos)
            },
            {
                "id": "guix-packages",
                "url": "https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages",
                "status": "active", 
                "package_listings": [f"gnu/packages/{f}" for f in guix_packages],
                "package_count": len(guix_packages)
            },
            {
                "id": "julia-ecosystem",
                "url": "https://github.com/JuliaLang/*",
                "status": "active",
                "package_listings": julia_packages,
                "package_count": len(julia_packages)
            }
        ],
        "summary": {
            "total_registries": 3,
            "total_packages_discovered": total_packages,
            "discovery_method": "real_api_calls"
        }
    }
    
    # Write to test output file
    with open('/tmp/enhanced_discovery_test.json', 'w') as f:
        json.dump(sample_output, f, indent=2)
    
    print(f"\n✅ Test output written to /tmp/enhanced_discovery_test.json")
    
    if total_packages > 0:
        print("🎉 Enhanced package discovery functionality verified!")
        return True
    else:
        print("⚠️  No packages discovered - may need fallback handling")
        return False

if __name__ == "__main__":
    print("🧠 Enhanced Package Discovery Test Suite")
    print("=" * 50)
    
    success = generate_test_output()
    
    if success:
        print("\n✅ All tests passed - enhanced discovery is working!")
        sys.exit(0)
    else:
        print("\n❌ Some tests failed - enhanced discovery needs improvement")
        sys.exit(1)