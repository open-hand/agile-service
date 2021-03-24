import React, { memo } from 'react';
import { observer } from 'mobx-react-lite';
import { Button, Tabs, Tooltip } from 'choerodon-ui/pro/lib';
import { useReleaseDetailContext } from '../stores';
import Detail from './detail';
import './Body.less';
import StoryTable from './story-table';
import BugTable from './bug-table';

const { TabPane } = Tabs;
const Body: React.FC = () => {
  const {
    prefixCls, disabled, store, projectId, events,
  } = useReleaseDetailContext();

  return (
    <Tabs className={`${prefixCls}-body`}>
      <TabPane key="detail" tab="详情">
        <Detail />
      </TabPane>
      <TabPane key="story" tab="完成的故事">
        <StoryTable />
      </TabPane>
      <TabPane key="bug" tab="解决的缺陷">
        <BugTable />
      </TabPane>
    </Tabs>
  );
};
export default observer(Body);
