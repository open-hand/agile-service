import { useState, useLayoutEffect } from 'react';
import ReactDOM from 'react-dom';
import PropTypes from 'prop-types';

function getContainer(target) {
  if (typeof target === 'object' && Object.prototype.hasOwnProperty.call(target, 'current')) {
    return target.current;
  }
  return typeof target === 'function' ? target() : target;
}
function Portal({ children, target }) {
  const container = getContainer(target);
  const [mount, setMount] = useState(container);
  useLayoutEffect(() => {
    setMount(true);
  }, []);
  if (!mount) {
    return null;
  }
  return ReactDOM.createPortal(children, container);
}
Portal.propTypes = {
  children: PropTypes.any,
  target: PropTypes.oneOf([
    PropTypes.func,
    PropTypes.element,
    PropTypes.shape({
      current: PropTypes.any,
    }),
  ]),
};
export default Portal;
