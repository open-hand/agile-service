import React from 'react';
import { Tabs } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import Description from '../description';
import Fields from '../fields';
import SubTask from '../sub-task';
import SubBug from '../sub-bug';
import Attachment from '../attachment';
import Comments from '../comments';
import Logs from '../logs';
import styles from './index.less';
import { useDetailContext } from '../../context';

const { TabPane } = Tabs;
const Body: React.FC = () => {
  const { store } = useDetailContext();
  const { issueTypeVO } = store.issue;
  return (
    <div
      className={styles.body}
    >
      <Tabs>
        <TabPane tab="详情" key="fields">
          <Fields />
          <Description />
          <Attachment />
          {issueTypeVO?.typeCode && ['issue_epic', 'sub_task', 'feature'].indexOf(issueTypeVO.typeCode) === -1
            ? <SubTask /> : ''}
          {issueTypeVO?.typeCode && ['story', 'task'].indexOf(issueTypeVO.typeCode) !== -1
            ? <SubBug /> : ''}
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
};

export default observer(Body);
