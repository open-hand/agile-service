/* eslint-disable react/self-closing-comp,jsx-a11y/accessible-emoji */
import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import { EmptyPage } from '@choerodon/components';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import EmptyScrumboard from './emptyScrumboard.svg';

const NoneSprint = ({ doingSprintExist, filterItems, hasSetFilter }) => {
  let tipTitle = '没有活跃的Sprint';
  const { sprint } = filterItems;
  if ((doingSprintExist || sprint) && Object.keys(filterItems).length === 1) {
    tipTitle = '当前冲刺下未规划工作项';
  } else if (hasSetFilter) {
    tipTitle = '当前筛选条件下无工作项';
  }

  const handleLinkToBacklog = useCallback(() => {
    to(LINK_URL.workListBacklog);
  }, []);

  return (
    <>
      <EmptyPage
        image={EmptyScrumboard}
        description={(
          <>
            {tipTitle}
            ，
            在工作列表的
            <EmptyPage.Button style={{ color: '#5365EA' }} onClick={handleLinkToBacklog}>待办事项</EmptyPage.Button>
            {!doingSprintExist ? '中开启冲刺' : '规划工作项到当前冲刺'}
          </>
        )}
      >
      </EmptyPage>
    </>
  );
};

export default observer(NoneSprint);
