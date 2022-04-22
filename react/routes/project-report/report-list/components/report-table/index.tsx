import React, { useMemo, useCallback } from 'react';
import {
  DataSet,
  Table,
  Modal,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { TableColumnTooltip } from 'choerodon-ui/pro/lib/table/enum';
import { EmptyPage } from '@choerodon/components';
import { projectReportApiConfig, projectReportApi } from '@/api';
import TableDropMenu from '@/components/table-drop-menu';
import Users from '@/components/tag/users';
import { User } from '@/common/types';
import to from '@/utils/to';
import Loading from '@/components/Loading';
import UserTag from '@/components/tag/user-tag';
import NoReport from './NoReport.svg';
import useFormatMessage from '@/hooks/useFormatMessage';

import styles from './index.less';

const { Column } = Table;

interface ReportTableProps {
  onClick: () => void
}
const ReportTable: React.FC<ReportTableProps> = ({ onClick }) => {
  const formatMessage = useFormatMessage();
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
        label: formatMessage({ id: 'agile.common.title' }),
      },
      {
        name: 'receiverList',
        type: 'array' as FieldType,
        label: formatMessage({ id: 'agile.projectReport.receiver' }),
      },
      {
        name: 'createdUser',
        type: 'object' as FieldType,
        label: formatMessage({ id: 'agile.common.creator' }),
      },
    ],
    queryFields: [{
      name: 'title',
      type: 'string' as FieldType,
      label: formatMessage({ id: 'agile.common.title' }),
    }],
  }), []);
  const handleMenuClick = useCallback(async (key, record) => {
    switch (key) {
      case 'delete': {
        Modal.open({
          title: '确认删除',
          children: `确认删除报告“${record.get('title')}”`,
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
          <TableDropMenu
            menuData={[{ action: () => to(`/agile/project-report/edit/${record?.get('id')}`), text: '修改' },
              {
                key: 'delete',
                text: '删除',
              }]}
            onMenuClick={({ key }: { key: string }) => handleMenuClick(key, record)}
            text={record?.get('title')}
          />
        )}
      />
      <Column
        name="receiverList"
        renderer={({ value: receiverList }) => (
          <div className={styles.userTagWrap}>
            <Users data={receiverList} />
            {receiverList?.length === 1 && (
              <span>{receiverList[0]?.realName}</span>
            )}
          </div>
        )}
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
    <EmptyPage
      description={(
        <>
          {formatMessage({ id: 'agile.projectReport.noReport' })}
          <EmptyPage.Button
            onClick={onClick}
          >
            【
            {formatMessage({ id: 'agile.projectReport.create' })}
            】
          </EmptyPage.Button>
        </>
      )}
      image={NoReport}
    />
  )
  );
};
export default observer(ReportTable);
