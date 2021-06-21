import React from 'react';
import ReactDOM from 'react-dom';
import Draggable from 'react-draggable';
import Portal from '@/components/Portal';
import './Modal.less';

const prefix = 'c7nagile-modal';
const map = new Map();
function Modal({
  header, content, footer, zIndex,
}) {
  return (
    <Draggable handle=".handle">
      <div className="c7nagile-modal" style={{ zIndex: zIndex || 1000 }}>
        <div className={`${prefix}-header handle`}>
          {header}
        </div>
        <div>
          {content}
        </div>
        <div className={`${prefix}-footer`}>
          {footer}
        </div>
      </div>
    </Draggable>
  );
}
Modal.close = (key) => {
  const target = map.get(key);
  if (!target) {
    return;
  }
  map.delete(key);
  const unmountResult = ReactDOM.unmountComponentAtNode(target);
  if (unmountResult && target.parentNode) {
    target.parentNode.removeChild(target);
  }
};
Modal.open = (props) => {
  const { key } = props;
  if (!map.get(key)) {
    const div = document.createElement('div');
    map.set(key, div);
    document.body.appendChild(div);

    ReactDOM.render(<Modal {...props} />, div);
  }
  return {
    close: () => {
      Modal.close(key);
    },
  };
};
export default Modal;
