import React from 'react';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/boot';
import Gantt from '@/components/gantt';

const GanttPage: React.FC = () => (
  <Page>
    <Header>
      <Button
        icon="playlist_add"
      >
        创建问题
      </Button>
    </Header>
    <Breadcrumb />
    <Content style={{ borderTop: '1px solid rgb(216, 216, 216)', display: 'flex', flexDirection: 'column' }}>

      <Gantt />
    </Content>
  </Page>
);
export default observer(GanttPage);
