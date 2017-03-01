import './index.css';
import '../node_modules/todomvc-common/base.css';
import '../node_modules/todomvc-app-css/index.css';

// This has the side effect of running the haskell main after all javascript is loaded
import {} from '../build/todo';
