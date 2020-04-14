import React, {
  useState, useCallback, useRef, useEffect,
} from 'react';
import { Dropdown, Button, Icon } from 'choerodon-ui/pro';

function useClickOut(onClickOut) {
  const ref = useRef();
  const handleClick = useCallback((e) => {
    if (!ref.current.contains(e.target)) {
      onClickOut(e);
    }
  }, [onClickOut]);
  useEffect(() => {
    document.addEventListener('click', handleClick);
    return () => {
      document.removeEventListener('click', handleClick);
    };
  }, [handleClick]);
  return ref;
}
export default function () {
  const [count, setCount] = useState(0);
  const [hidden, setHidden] = useState(true);
  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const ref = useClickOut(handleClickOut);
  return (
    <div style={{ display: 'flex', flexWrap: 'wrap' }}>
      {Array(count).fill(0).map(i => <div style={{ width: 200 }}>{i}</div>)}
      <Dropdown
        hidden={hidden}
        overlay={(
          <div style={{ width: 200, height: 300, background: 'red' }} ref={ref}>
            <Button onClick={() => {
              setCount(count + 1);
            }}
            >
              add
              {count}
            </Button>
          </div>
        )}
        trigger={['click']}
      >
        <Button onClick={(e) => {
          e.nativeEvent.stopImmediatePropagation();
          setHidden(false);
        }}
        >
          添加筛选
          <Icon type="arrow_drop_down" />
        </Button>
      </Dropdown>
    </div>
  );
}
