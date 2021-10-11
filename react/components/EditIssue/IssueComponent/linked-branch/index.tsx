// @ts-nocheck
import React, {
  useCallback, useMemo, useImperativeHandle, forwardRef,
} from 'react';
import {
  Table, DataSet, Modal,
} from 'choerodon-ui/pro';
import {
  Popover,
} from 'choerodon-ui';
import { TableColumnTooltip, TableQueryBarType } from 'choerodon-ui/pro/lib/table/enum';
import { devOpsApi, devOpsApiConfig } from '@/api';
import TableDropMenu from '@/components/table-drop-menu';
import { map } from 'lodash';

const { Column } = Table;

const STATUS_SHOW = {
  opened: '开放',
  merged: '已合并',
  closed: '关闭',
};
function getStatus(mergeRequests: any[]) {
  if (!mergeRequests.length) {
    return '';
  }
  const statusArray = map(mergeRequests, 'state');
  if (statusArray.includes('opened')) {
    return '开放';
  }
  if (statusArray.includes('merged')) {
    return '已合并';
  }
  return '关闭';
}
const LinkedBranch: React.ForwardRefRenderFunction<{
  issueId: string
}> = ({ issueId }, ref) => {
  const dataSet = useMemo(() => new DataSet({
    autoQuery: true,
    selection: false,
    paging: false,
    transport: {
      read: ({ params }) => devOpsApiConfig.loadCommit(issueId),
    },
    fields: [{
      name: 'branchName',
      label: '分支',
    }, {
      name: 'appServiceName',
      label: '应用服务名称',
    }, {
      name: 'projectName',
      label: '所属项目',
    }, {
      name: 'originBranch',
      label: '来源分支',
    }, {
      name: 'commits',
      label: '提交数',
    }, {
      name: 'status',
      label: '状态',
    }],
  }), [issueId]);
  const handleCreateMergeRequest = (record) => {
    const win = window.open('');
    const { appServiceId, projectId } = record;
    devOpsApi.project(projectId).loadGitUrl(appServiceId)
      .then((res) => {
        const url = `${res}/merge_requests/new?change_branches=true&merge_request[source_branch]=${record.branchName}&merge_request[target_branch]=master`;
        win.location.href = url;
      })
      .catch((error) => {
      });
  };
  const handleMenuClick = useCallback(async (key, r) => {
    const record = r.toData();
    switch (key) {
      case 'delete': {
        Modal.open({
          title: '移除关联分支',
          children: `确认删除“${record.branchName}”分支与工作项的关联？关系移除后，可能会移除此工作项的部分Tag信息，并且无法自动恢复Tag信息`,
          okText: '移除',
          onOk: async () => {
            await devOpsApi.project(record.projectId).removeLinkBranch(record.appServiceId, record.id, issueId);
            dataSet.query();
          },
        });
        break;
      }
      case 'merge': {
        handleCreateMergeRequest(record);
        break;
      }
      default: break;
    }
  }, [dataSet, issueId]);
  useImperativeHandle(ref, () => ({
    query: () => dataSet.query(),
  }));
  return (

    <Table dataSet={dataSet} queryBar={'none' as TableQueryBarType}>
      <Column
        width={210}
        name="branchName"
        tooltip={'overflow' as TableColumnTooltip}
        renderer={({ record, text }) => (
          <TableDropMenu
            onMenuClick={({ key }: { key: string }) => handleMenuClick(key, record)}
            menuData={[{
              key: 'delete',
              text: '移除关联分支',
            }, {
              key: 'merge',
              text: '创建合并请求',
            }]}
            text={text}
          />
        )}
      />
      <Column tooltip={'overflow' as TableColumnTooltip} className="c7n-agile-table-cell" name="appServiceName" />
      <Column tooltip={'overflow' as TableColumnTooltip} className="c7n-agile-table-cell" name="projectName" />
      <Column tooltip={'overflow' as TableColumnTooltip} className="c7n-agile-table-cell" name="originBranch" />
      <Column width={70} className="c7n-agile-table-cell" name="commits" renderer={({ record }) => record?.get('commits')?.length} />
      <Column
        width={80}
        className="c7n-agile-table-cell"
        name="status"
        renderer={({ record: r }) => {
          if (!r) {
            return null;
          }
          const record = r.toData();
          return (
            <div style={{ width: '100%', overflow: 'hidden' }}>
              <Popover
                overlayStyle={{
                  boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0), 0 8px 10px 1px rgba(0, 0, 0, 0), 0 3px 14px 2px rgba(0, 0, 0, 0)',
                }}
                content={(
                  <div>
                    {
                      record.mergeRequests && record.mergeRequests.length ? (
                        <ul>
                          {
                            record.mergeRequests.map((v: any) => (
                              <li style={{ listStyleType: 'none' }}>
                                <span style={{ display: 'inline-block', width: 150 }}>{v.title}</span>
                                <span style={{ display: 'inline-block', width: 50 }}>{['opened', 'merged', 'closed'].includes(v.state) ? STATUS_SHOW[v.state as string] : ''}</span>
                              </li>
                            ))
                          }
                        </ul>
                      ) : <div>暂无相关合并请求</div>
                    }
                  </div>
                )}
              >
                <p style={{
                  overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap', marginBottom: 0,
                }}
                >
                  {getStatus(record.mergeRequests)}
                </p>
              </Popover>
            </div>
          );
        }}
      />
    </Table>
  );
};

export default forwardRef(LinkedBranch);
