import React from 'react';
import {
  Page, Header, Breadcrumb, Content,
} from '@choerodon/boot';
import { Button } from 'choerodon-ui/pro';
import openAddModal from './components/add-modal';

const CreateReport: React.FC = () => (
  <Page>
    <Header>
      <Button icon="playlist_add">添加报告内容</Button>
    </Header>
    <Breadcrumb />
    <Content>
      CreateReport
      <Button onClick={() => {
        openAddModal({
          type: 'chart',
        });
      }}
      >
        openAddModal
      </Button>
    </Content>
  </Page>
);

export default CreateReport;
