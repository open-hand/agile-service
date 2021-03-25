import React, { useCallback, useMemo } from 'react';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/master';
import {
  Button, Table, DataSet, Modal,
} from 'choerodon-ui/pro';
import { TableColumnTooltip } from 'choerodon-ui/pro/lib/table/enum';
import { kanbanTemplateApiConfig, kanbanTemplateApi } from '@/api';
import TableAction from '@/components/TableAction';
import to from '@/utils/to';
import openKanbanTemplateModal from './components/modal';

const { Column } = Table;

const KanbanTemplateList = () => {
  const dataSet = useMemo(() => new DataSet({
    data: [{
      name: '1',
      description: 'aa',
    }],
    transport: {
      read: ({ params }) => kanbanTemplateApiConfig.list(params.page, params.size),
    },
    fields: [{
      name: 'name',
      label: '看板名称',
    }, {
      name: 'description',
      label: '描述',
    }],
  }), []);
  const handleClick = useCallback(() => {
    openKanbanTemplateModal({ mode: 'create' });
  }, []);
  const handleMenuClick = useCallback(async (key, record) => {
    switch (key) {
      case 'delete': {
        Modal.confirm({
          title: `确认删除报告“${record.get('title')}”`,
          onOk: async () => {
            await kanbanTemplateApi.delete(record.get('boardId'));
            dataSet.query();
          },
        });
        break;
      }
      case 'detail': {
        to(`/agile/kanban-template/detail/${record.get('boardId')}`, {
          type: 'org',
        });
        break;
      }
      default: break;
    }
  }, [dataSet]);
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
          <Column
            name="name"
            tooltip={'overflow' as TableColumnTooltip}
            renderer={({ record, text }) => (
              <TableAction
                onEditClick={() => record && openKanbanTemplateModal({
                  mode: 'edit',
                  boardId: record.get('boardId'),
                  data: {
                    name: record.get('name'),
                    description: record.get('description'),
                  },
                })}
                onMenuClick={({ key }: { key: string }) => handleMenuClick(key, record)}
                menus={[{
                  key: 'detail',
                  text: '编辑模板',
                }, {
                  key: 'delete',
                  text: '删除',
                }]}
                text={text}
              />
            )}
          />
          <Column name="description" />
        </Table>
      </Content>
    </Page>
  );
};

export default KanbanTemplateList;
