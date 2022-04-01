import React, {
  useCallback, useMemo, useState, useEffect,
} from 'react';
import {
  Page, Header, Content, Breadcrumb, useTheme, HeaderButtons,
} from '@choerodon/master';
import {
  Button, Table, DataSet, Modal, Tooltip,
} from 'choerodon-ui/pro';
import { TableColumnTooltip } from 'choerodon-ui/pro/lib/table/enum';
import { kanbanTemplateApiConfig, kanbanTemplateApi } from '@/api';
import TableDropMenu from '@/components/table-drop-menu';
import to from '@/utils/to';
import UserHead from '@/components/UserHead';
import openKanbanTemplateModal from './components/modal';

const { Column } = Table;

const KanbanTemplateList = () => {
  const [enableCreate, setEnableCreate] = useState(false);
  useEffect(() => {
    (async () => {
      const { statusMachineTemplateConfig } = await kanbanTemplateApi.checkEnableCreate();
      setEnableCreate(statusMachineTemplateConfig);
    })();
  }, []);
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
        Modal.open({
          title: '删除看板模板',
          children: `确认删除看板模板“${record.get('name')}”`,
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

  const [theme] = useTheme();
  return (
    <Page>
      <Header>
        <HeaderButtons items={[{
          name: '创建看板模板',
          icon: 'playlist_add',
          disabled: !enableCreate,
          handler: handleClick,
          tooltipsConfig: {
            title: enableCreate ? undefined : '请先配置故事、任务、子任务或缺陷的状态机模板',
          },
          display: true,
        }]}
        />
      </Header>
      <Breadcrumb />
      <Content style={theme === 'theme4' ? undefined : { paddingTop: 0 }}>
        <Table dataSet={dataSet}>
          <Column
            name="name"
            tooltip={'overflow' as TableColumnTooltip}
            renderer={({ record, text }) => (
              <TableDropMenu
                menuData={[{
                  action: () => record && to(`/agile/states/kanban-template/detail/${record.get('boardId')}`, {
                    type: 'org',
                  }),
                  text: '修改模板',
                }, {
                  key: 'detail',
                  text: '修改',
                }, {
                  key: 'delete',
                  text: '删除',
                }]}
                onMenuClick={({ key }: { key: string }) => handleMenuClick(key, record)}
                text={text}
              />
            )}
          />
          <Column className="c7n-agile-table-cell" name="creator" renderer={({ record }) => <UserHead style={{ display: 'inline-flex' }} user={record?.get('creator')} />} />
          <Column className="c7n-agile-table-cell" name="creationDate" />
        </Table>
      </Content>
    </Page>
  );
};

export default KanbanTemplateList;
