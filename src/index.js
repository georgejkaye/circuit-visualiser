import _ from 'lodash';

function component() {
    const element = document.createElement('div');
  
    element.innerHTML = "Welcome aboard this West Midlands Railway service to Lichfield Trent Valley!";//_.join(['Hello', 'webpack'], ' ');
  
    return element;
  }
  
  document.body.appendChild(component());