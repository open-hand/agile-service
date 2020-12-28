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

const { TabPane } = Tabs;
const GroupPerformance = () => {
  const { prefixCls } = useContext(Store);

  return (
    <Page className={prefixCls}>
      <Breadcrumb />
      <Content>
        <Tabs animated={false}>
          <TabPane key="efficiency" tab="进度与效率">
            <div className={`${prefixCls}-container`}>
              <Efficiency />
            </div>
          </TabPane>
          <TabPane key="quality" tab="质量分析">
            <div className={`${prefixCls}-container`}>
              <Quality />
            </div>
          </TabPane>
          <TabPane key="tendency" tab="趋势分析">
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
