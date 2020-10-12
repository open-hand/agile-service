import React, {
  useState, useCallback, useRef, useEffect, useMemo,
} from 'react';
import {
  Button, Icon, DataSet, 
} from 'choerodon-ui/pro';
import { Dropdown } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import StoryMapStore from '@/stores/project/StoryMap/StoryMapStore';
import StoryFilter from '../StoryFilter';
import SelectDataSet from '../../store/selectDataSet';
import './index.less';
  
function useClickOut(onClickOut) {
  const ref = useRef();
  
  const handleClick = useCallback((e) => {
    const popupContainerEles = document.getElementsByClassName('c7n-pro-popup-container');
    const triggerBtn = document.getElementsByClassName('c7nagile-StoryMap-StoryFilterDropDown-triggerBtn')[0];
    let allIsNotContain = true;
    for (let i = 0; i < popupContainerEles.length; i += 1) {
      if (popupContainerEles[i].contains(e.target)) {
        allIsNotContain = false;
        break;
      }
    }
    if (ref.current && (!ref.current.contains(e.target) && allIsNotContain && e.target.tagName !== 'BODY' && !triggerBtn.contains(e.target))) {
      onClickOut(e);
    }
  }, [onClickOut]);
  useEffect(() => {
    document.addEventListener('click', handleClick, true);
    return () => {
      document.removeEventListener('click', handleClick, true);
    };
  }, [handleClick]);
  return ref;
}
    
function StoryFilterDropDown() {  
  const [hidden, setHidden] = useState(true);  
  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const ref = useClickOut(handleClickOut);
  const selectDataSet = useMemo(() => new DataSet(SelectDataSet(StoryMapStore)), []);
  const {
    isCompleted, components, sprints, prioritys, 
  } = (selectDataSet.current && selectDataSet.current.data) || {};
  const hasFilter = (isCompleted || isCompleted === false) || components || sprints || prioritys;
  return (
    <div
      style={{ marginLeft: 5, display: 'flex', alignItems: 'center' }}
    >
      <Dropdown
        className="c7nagile-StoryMap-StoryFilterDropDown"
        getPopupContainer={(trigger) => trigger.parentNode}
        visible={!hidden}
        overlay={(
          <div            
            ref={ref}
            onClick={(e) => {
              e.stopPropagation();
            }}
          >
            <StoryFilter selectDataSet={selectDataSet} hasFilter={hasFilter} />
          </div>
            )}
        trigger={['click']}
      >
        <Button
          className="c7nagile-StoryMap-StoryFilterDropDown-triggerBtn"
          onClick={(e) => {
            e.nativeEvent.stopImmediatePropagation();
            setHidden(!hidden);
          }}
        >
          故事筛选
          <span className="c7nagile-StoryMap-StoryFilterDropDown-triggerBtn-tip">
            <Icon type="filter2" />
            {
                  hasFilter && (
                  <span className="c7nagile-StoryMap-StoryFilterDropDown-triggerBtn-tip-circle" />
                  )
              }
          </span>
        </Button>
      </Dropdown>
    </div>    
  );
}
export default observer(StoryFilterDropDown);
