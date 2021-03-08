import React, { memo } from 'react';
import { observer } from 'mobx-react-lite';
import { Button, Tabs, Tooltip } from 'choerodon-ui/pro/lib';
import { useReleaseDetailContext } from '../stores';
import Detail from './detail';
import './Body.less';

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
        完成的故事
      </TabPane>
      <TabPane key="bug" tab="解决的缺陷">
        解决的缺陷
      </TabPane>
    </Tabs>
  );
};
export default observer(Body);
