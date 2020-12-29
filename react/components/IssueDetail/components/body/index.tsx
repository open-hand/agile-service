import React from 'react';
import { Tabs } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import Description from '../description';
import Fields from '../fields';
import Attachment from '../attachment';
import Comments from '../comments';
import Logs from '../logs';
import styles from './index.less';

const { TabPane } = Tabs;
const Body: React.FC = () => (
  <div
    className={styles.body}
  >
    <Tabs>
      <TabPane tab="详情" key="fields">
        <Fields />
        <Description />
        <Attachment />
      </TabPane>
      <TabPane tab="评论" key="comment">
        <Comments />
      </TabPane>
      <TabPane tab="记录" key="record">
        <Logs />
      </TabPane>
    </Tabs>
  </div>
);

export default observer(Body);
