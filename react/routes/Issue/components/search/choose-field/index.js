import React, {
  useState, useCallback, useRef, useEffect,
} from 'react';
import {
  Dropdown, Button, Icon,  
} from 'choerodon-ui/pro';
import IssueStore from '@/stores/project/sprint/IssueStore';
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
    <div style={{ marginLeft: 5, marginTop: 8 }}>
      <Dropdown      
        hidden={hidden}
        overlay={(
          <div            
            ref={ref}
          >
            <FieldList />
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
export default observer(ChooseField);
