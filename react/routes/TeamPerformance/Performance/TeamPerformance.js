import React, { useContext } from 'react';
import { Tabs } from 'choerodon-ui/pro';
import {
  Page, Content, Breadcrumb,
} from '@choerodon/boot';
import Efficiency from '../components/Efficiency';
import Quality from '../components/Quality';
import Tendency from '../components/Tendency';
import Store from '../stores';
import './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

const { TabPane } = Tabs;
const GroupPerformance = () => {
  const { prefixCls } = useContext(Store);
  const formatMessage = useFormatMessage('agile.performance');
  return (
    <Page className={prefixCls}>
      <Breadcrumb />
      <Content>
        <Tabs animated={false}>
          <TabPane key="efficiency" tab={formatMessage({ id: 'progress.efficiency' })}>
            <div className={`${prefixCls}-container`}>
              <Efficiency />
            </div>
          </TabPane>
          <TabPane key="quality" tab={formatMessage({ id: 'quality' })}>
            <div className={`${prefixCls}-container`}>
              <Quality />
            </div>
          </TabPane>
          <TabPane key="tendency" tab={formatMessage({ id: 'trend' })}>
            <div className={`${prefixCls}-container`}>
              <Tendency />
            </div>
          </TabPane>
        </Tabs>
      </Content>
    </Page>
  );
};

export default GroupPerformance;
