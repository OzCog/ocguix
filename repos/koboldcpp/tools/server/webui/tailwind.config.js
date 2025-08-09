/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {},
  },
  plugins: [
    require('daisyui'),
  ],
  daisyui: {
    themes: [
      'light', 
      'dark',
      // OpenCog Custom Themes
      {
        'opencog-neural': {
          'primary': '#4c6ef5',          // Neural Blue - for primary actions and highlights
          'primary-focus': '#364fc7',     // Deeper Neural Blue
          'primary-content': '#ffffff',   
          
          'secondary': '#51cf66',         // Synapse Green - for success states and active connections
          'secondary-focus': '#37b24d',   
          'secondary-content': '#ffffff',
          
          'accent': '#9775fa',            // Cognition Purple - for special cognitive elements
          'accent-focus': '#7950f2',      
          'accent-content': '#ffffff',
          
          'neutral': '#495057',           // Neutral Gray for balanced elements
          'neutral-focus': '#343a40',     
          'neutral-content': '#ffffff',
          
          'base-100': '#ffffff',          // Light background
          'base-200': '#f8f9fa',          
          'base-300': '#e9ecef',          
          'base-content': '#212529',      // Dark text on light background
          
          'info': '#339af0',              // Information Blue
          'success': '#51cf66',           // Success Green (matches secondary)
          'warning': '#ffd43b',           // Warning Yellow
          'error': '#ff6b6b',             // Error Red
          
          '--rounded-box': '0.5rem',
          '--rounded-btn': '0.25rem',
          '--rounded-badge': '1.9rem',
          '--animation-btn': '0.25s',
          '--animation-input': '0.2s',
          '--btn-focus-scale': '0.95',
          '--border-btn': '1px',
          '--tab-border': '1px',
          '--tab-radius': '0.5rem',
        },
      },
      {
        'opencog-quantum': {
          'primary': '#3b82f6',           // Quantum Blue - deeper, more mysterious
          'primary-focus': '#2563eb',     
          'primary-content': '#ffffff',
          
          'secondary': '#10b981',         // Quantum Green - for active neural connections
          'secondary-focus': '#059669',   
          'secondary-content': '#ffffff',
          
          'accent': '#8b5cf6',            // Quantum Purple - for advanced cognitive functions
          'accent-focus': '#7c3aed',      
          'accent-content': '#ffffff',
          
          'neutral': '#1f2937',           // Dark Neutral for depth
          'neutral-focus': '#111827',     
          'neutral-content': '#f9fafb',
          
          'base-100': '#0f172a',          // Deep Dark background
          'base-200': '#1e293b',          
          'base-300': '#334155',          
          'base-content': '#f1f5f9',      // Light text on dark background
          
          'info': '#0ea5e9',              
          'success': '#10b981',           
          'warning': '#f59e0b',           
          'error': '#ef4444',             
          
          '--rounded-box': '0.75rem',
          '--rounded-btn': '0.5rem',
          '--rounded-badge': '1.9rem',
          '--animation-btn': '0.25s',
          '--animation-input': '0.2s',
          '--btn-focus-scale': '0.95',
          '--border-btn': '1px',
          '--tab-border': '1px',
          '--tab-radius': '0.75rem',
        },
      },
      // Include other default themes
      'cupcake', 'bumblebee', 'emerald', 'corporate', 'synthwave', 'retro', 'cyberpunk', 'valentine', 'halloween', 'garden', 'forest', 'aqua', 'lofi', 'pastel', 'fantasy', 'wireframe', 'black', 'luxury', 'dracula', 'cmyk', 'autumn', 'business', 'acid', 'lemonade', 'night', 'coffee', 'winter', 'dim', 'nord', 'sunset'
    ],
  }
}
