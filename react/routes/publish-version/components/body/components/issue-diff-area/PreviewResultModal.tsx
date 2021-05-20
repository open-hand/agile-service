import React, {
  useCallback,
  useEffect, useMemo,
} from 'react';
import {
  Button, Table,
  DataSet, Modal, Tooltip,
} from 'choerodon-ui/pro/lib';
// @ts-ignore
import JSONbig from 'json-bigint';
import { Choerodon } from '@choerodon/boot';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import DetailContainer, { useDetail } from '@/components/detail-container';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { IModalProps } from '@/common/types';
import UserTag from '@/components/tag/user-tag';
import renderStatus from '@/components/column-renderer/status';
import renderPriority from '@/components/column-renderer/priority';
import renderTags from '@/components/column-renderer/tags';
import { getProjectId, getOrganizationId } from '@/utils/common';
import IssueSearch, { useIssueSearchStore } from '@/components/issue-search';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import renderSummary from '@/components/column-renderer/summary';
import { transformFilter, issuesFilter } from './utils';
import styles from './PreviewResultModal.less';

const JSONbigString = JSONbig({ storeAsString: true });

const { Column } = Table;
interface PreviewResultModalProps {
  publishVersionId: string
  tableData: any[]
  handleOk?: () => any
  onChangeIssueTag: (action: 'add' | 'update') => void
  selectIssue: (issueId: string) => void
}
const PreviewResult: React.FC<{ modal?: IModalProps } & PreviewResultModalProps> = ({
  modal, tableData, handleOk, selectIssue,
}) => {
  const [detailProps] = useDetail();

  const issueSearchStore = useIssueSearchStore({
    getSystemFields: () => getSystemFields()
      .filter((i) => ['contents', 'issueTypeId', 'priorityId', 'statusId', 'assigneeId'].includes(i.code)) as any,
    transformFilter,
  });
  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: false,
    paging: true,
    selection: false,
    data: tableData,

    fields: [
      { name: 'summary', label: '概要' },
      { name: 'issueNum', label: '编号' },
      { name: 'status', label: '状态' },
      { name: 'priority', label: '优先级' },
      // { name: 'influenceVersion', label: '影响的版本' },

      { name: 'assigneeId', label: '经办人' },
      { name: 'creationDate', label: '创建时间' },
      { name: 'tags', label: 'Tag' },

    ],

  }), [tableData]);

  // const handleSubmit = useCallback(async () => {
  //   if (!await ds.current?.validate()) {
  //     return false;
  //   }

  //   // await ds.submit();
  //   const result = handleOk && await handleOk();
  //   return typeof (result) !== 'undefined' ? result : true;
  // }, [ds, handleOk]);
  // useEffect(() => {
  //   modal?.handleOk(handleSubmit);
  // }, [handleSubmit, modal]);
  // function renderSummary({ value }: RenderProps) {
  //   return (
  //     <Tooltip title={value} placement="topLeft">
  //       <span className="c7n-agile-table-cell-click">{value}</span>
  //     </Tooltip>
  //   );
  // }
  return (
    <div className={styles.wrap}>
      <IssueSearch
        store={issueSearchStore}
        onClear={() => { }}
        onChange={() => {
          console.log('change', issueSearchStore.getCustomFieldFilters());
          ds.loadData(issuesFilter(tableData, issueSearchStore.getCustomFieldFilters()));
        }}
      />
      <Table dataSet={ds} className={styles.table}>
        <Column
          name="summary"
          className="c7n-agile-table-cell-click"
          onCell={({ record }) => ({
            onClick: () => {
              detailProps.open({
                path: 'issue',
                props: {
                  disabled: true,
                  issueId: record.get('issueId'),
                  projectId: getProjectId(),
                  organizationId: getOrganizationId(),
                },
                events: {
                  update: () => {
                    // issueInfoTableDataSet.query(issueInfoTableDataSet.currentPage);
                    // issueDiffDataSet.query(issueDiffDataSet.currentPage)
                    // handleRefresh();
                  },
                },
              });
            },
          })}
          lock={'left' as any}
          width={360}
          renderer={renderSummary}
        />
        <Column name="issueNum" width={120} tooltip={'overflow' as any} className="c7n-agile-table-cell" />
        <Column name="status" renderer={({ record }) => (record?.get('statusVO') ? renderStatus({ record }) : undefined)} />
        <Column name="priority" width={150} renderer={renderPriority} />
        <Column
          name="assigneeId"
          className="c7n-agile-table-cell"
          renderer={({ value, record }) => (value ? (
            <UserTag data={{
              imageUrl: record?.get('assigneeImageUrl'),
              loginName: record?.get('assigneeLoginName'),
              realName: record?.get('assigneeRealName'),
              tooltip: record?.get('assigneeName'),
            }}
            />
          ) : '')}
        />
        <Column name="tags" width={150} tooltip={'overflow' as any} renderer={({ value }) => value?.map((i:any) => `${i.appServiceCode}:${i.tagName}`).join('、')} />
        <Column name="creationDate" className="c7n-agile-table-cell" width={120} renderer={({ value }) => (value ? String(value).split(' ')[0] : '')} />

      </Table>
      <DetailContainer {...detailProps} />
    </div>
  );
};
function openPreviewResultModal(props: PreviewResultModalProps) {
  const key = Modal.key();
  const { handleOk } = props;
  let modal = {} as any;
  function handleClose() {
    modal.close();
    return true;
  }
  function handleChangeIssueTag(action: 'add' | 'update') {
    props.onChangeIssueTag(action);

    Modal.confirm({
      title: '正在更新Tag',
      children: '正在更新tag，请进入查看版本信息查看进度',
      okText: '跳转至查看版本信息',
      onOk: () => {
        handleClose();
        handleOk && handleOk();
        return true;
      },
      onCancel: handleClose,
      cancelText: '关闭',
    });
    // modal.close();
  }
  modal = Modal.open({
    key,
    title: '预览结果',
    style: {
      width: MODAL_WIDTH.large,
    },
    drawer: true,
    cancelText: '关闭',
    footer: (okBtn: React.ReactNode, cancelBtn: React.ReactNode) => (
      <div>
        {/* {okBtn} */}

        <Button funcType={'raised' as any} color={'primary' as any} onClick={() => handleChangeIssueTag('add')}>增加问题tag信息</Button>
        {cancelBtn}
      </div>
    ),
    children: <PreviewResult {...props} />,

  });
}

export { openPreviewResultModal };
