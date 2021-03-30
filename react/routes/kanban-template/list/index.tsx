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
import UserHead from '@/components/UserHead';
import openKanbanTemplateModal from './components/modal';

const { Column } = Table;

const KanbanTemplateList = () => {
  const dataSet = useMemo(() => new DataSet({
    autoQuery: true,
    selection: false,
    transport: {
      read: ({ params }) => kanbanTemplateApiConfig.list(params.page, params.size),
    },
    fields: [{
      name: 'name',
      label: '看板名称',
    }, {
      name: 'creator',
      label: '创建人',
    }, {
      name: 'creationDate',
      label: '创建时间',
    }],
  }), []);
  const handleClick = useCallback(() => {
    openKanbanTemplateModal({ mode: 'create', onSubmit: () => dataSet.query() });
  }, [dataSet]);
  const handleMenuClick = useCallback(async (key, record) => {
    switch (key) {
      case 'delete': {
        Modal.confirm({
          title: `确认删除看板模板“${record.get('name')}”`,
          onOk: async () => {
            await kanbanTemplateApi.delete(record.get('boardId'));
            dataSet.query(dataSet.currentPage);
          },
        });
        break;
      }
      case 'detail': {
        openKanbanTemplateModal({
          mode: 'edit',
          data: {
            name: record.get('name'),
            objectVersionNumber: record.get('objectVersionNumber'),
            boardId: record.get('boardId'),
          },
          onSubmit: () => dataSet.query(),
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
        borderTop: 'none',
      }}
      >
        <Table dataSet={dataSet}>
          <Column
            name="name"
            tooltip={'overflow' as TableColumnTooltip}
            renderer={({ record, text }) => (
              <TableAction
                onEditClick={() => record && to(`/agile/kanban-template/detail/${record.get('boardId')}`, {
                  type: 'org',
                })}
                onMenuClick={({ key }: { key: string }) => handleMenuClick(key, record)}
                menus={[{
                  key: 'detail',
                  text: '编辑',
                }, {
                  key: 'delete',
                  text: '删除',
                }]}
                text={text}
              />
            )}
          />
          <Column name="creator" renderer={({ record }) => <UserHead user={record?.get('creator')} />} />
          <Column name="creationDate" />
        </Table>
      </Content>
    </Page>
  );
};

export default KanbanTemplateList;
