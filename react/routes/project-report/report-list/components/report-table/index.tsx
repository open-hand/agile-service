import React, { useMemo, useCallback } from 'react';
import {
  DataSet,
  Table,
  Modal,
  Button,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { TableColumnTooltip } from 'choerodon-ui/pro/lib/table/enum';
import { projectReportApiConfig, projectReportApi } from '@/api';
import TableAction from '@/components/TableAction';
import Users from '@/components/tag/users';
import { User } from '@/common/types';
import to from '@/utils/to';
import Empty from '@/components/Empty';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import Loading from '@/components/Loading';
import UserTag from '@/components/tag/user-tag';
import NoReport from './no-report.svg';

const { Column } = Table;

interface ReportTableProps {
  onClick: () => void
}
const ReportTable: React.FC<ReportTableProps> = ({ onClick }) => {
  const dataSet = useMemo(() => new DataSet({
    primaryKey: 'id',
    autoQuery: true,
    selection: false,
    pageSize: 10,
    transport: {
      read: () => projectReportApiConfig.load(),
    },
    fields: [
      {
        name: 'title',
        type: 'string' as FieldType,
        label: '标题',
      },
      {
        name: 'receiverList',
        type: 'array' as FieldType,
        label: '收件人',
      },
      {
        name: 'createdUser',
        type: 'object' as FieldType,
        label: '创建人',
      },
    ],
  }), []);
  const handleMenuClick = useCallback(async (key, record) => {
    switch (key) {
      case 'delete': {
        Modal.confirm({
          title: `确认删除报告“${record.get('title')}”`,
          onOk: async () => {
            await projectReportApi.delete(record.get('id'));
            dataSet.query();
          },
        });
      }
    }
  }, [dataSet]);
  // 首次加载
  if (dataSet.length === 0 && dataSet.status === 'loading') {
    return <Loading loading />;
  }
  return (dataSet.length > 0 ? (
    <Table
      key="user"
      dataSet={dataSet}
      autoMaxWidth
    >
      <Column
        name="title"
        tooltip={'overflow' as TableColumnTooltip}
        renderer={({ record }) => (
          <TableAction
            onEditClick={() => to(`/agile/project-report/edit/${record?.get('id')}`)}
            onMenuClick={({ key }: { key: string }) => handleMenuClick(key, record)}
            menus={[{
              key: 'delete',
              text: '删除',
            }]}
            text={record?.get('title')}
          />
        )}
      />
      <Column
        name="receiverList"
        renderer={({ value: receiverList }) => <Users data={receiverList} />}
      />
      <Column
        name="createdUser"
        renderer={({ value: createdUser }) => {
          if (!createdUser) {
            return null;
          }
          return (
            <UserTag
              style={{ display: 'inline-flex' }}
              data={createdUser as User}
            />
          );
        }}
      />
    </Table>
  ) : (
    <Empty
      title="暂无项目报告"
      description={(
        <>
          当前项目下无项目报告，请创建
          <br />
          <Button
            style={{ fontSize: '14px', marginTop: 15 }}
            color={'blue' as ButtonColor}
            funcType={'raised' as FuncType}
            onClick={onClick}
          >
            创建项目报告
          </Button>
        </>
        )}
      pic={NoReport}
    />
  )
  );
};
export default observer(ReportTable);
