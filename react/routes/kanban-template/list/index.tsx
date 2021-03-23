import React, { useCallback, useMemo } from 'react';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/master';
import { Button, Table, DataSet } from 'choerodon-ui/pro';
import openKanbanTemplateModal from './components/create';

const { Column } = Table;

const KanbanTemplateList = () => {
  const dataSet = useMemo(() => new DataSet({
    fields: [{
      name: 'name',
      label: '看板名称',
    }, {
      name: 'description',
      label: '描述',
    }],
  }), []);
  const handleClick = useCallback(() => {
    openKanbanTemplateModal({});
  }, []);
  return (
    <Page>
      <Header>
        <Button
          icon="playlist_add"
          onClick={handleClick}
        >
          创建看板模板
        </Button>
      </Header>
      <Breadcrumb />
      <Content style={{
        borderTop: '1px solid rgb(216, 216, 216)',
      }}
      >
        <Table dataSet={dataSet}>
          <Column name="name" />
          <Column name="description" />
        </Table>
      </Content>
    </Page>
  );
};

export default KanbanTemplateList;
