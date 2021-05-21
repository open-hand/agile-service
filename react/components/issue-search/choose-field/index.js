import React, {
  useState, useCallback, useRef, useEffect,
} from 'react';
import {
  Button, Icon,
} from 'choerodon-ui/pro';
import { Dropdown } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import FieldList from './FieldList';

function useClickOut(onClickOut) {
  const ref = useRef();
  const handleClick = useCallback((e) => {
    if (ref.current && !ref.current.contains(e.target)) {
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

function ChooseField() {
  const [hidden, setHidden] = useState(true);
  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const ref = useClickOut(handleClickOut);
  return (
    <div
      className="c7n-agile-issue-search-choose-field"
      style={{ marginLeft: 5, display: 'flex', alignItems: 'center' }}
    >
      <Dropdown
        getPopupContainer={(trigger) => trigger.parentNode}
        visible={!hidden}
        overlay={(
          <div
            role="none"
            ref={ref}
            onClick={(e) => {
              e.stopPropagation();
            }}
          >
            <FieldList />
          </div>
        )}
        trigger={['click']}
      >
        <Button
          style={{
            height: 32,
            paddingTop: 4,
          }}
          onClick={(e) => {
            e.nativeEvent.stopImmediatePropagation();
            setHidden(false);
          }}
        >

          添加筛选
          <Icon type="arrow_drop_down" style={{ marginTop: -3 }} />
        </Button>
      </Dropdown>
    </div>
  );
}
export default observer(ChooseField);
