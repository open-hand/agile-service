import React, { useEffect, useState } from 'react';
import TimeAgo from 'timeago-react';
import {
  Button, Icon, Popover, Tooltip,
} from 'choerodon-ui';
import { Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react-lite';
import { FormattedMessage } from 'react-intl';
import { devOpsApi } from '@/api';
import Commits from '../../../Commits';
import MergeRequest from '../../../MergeRequest';

const STATUS_SHOW = {
  opened: '开放',
  merged: '已合并',
  closed: '关闭',
};
const IssueBranch = observer(({
  store, disabled, projectId, reloadIssue, otherProject, outside, programId, applyType, issueId,
}) => {
  const [commitShow, setCommitShow] = useState(false);
  const [mergeRequestShow, setMergeRequestShow] = useState(false);
  useEffect(() => {
    (async () => {
      const res = await (otherProject || outside || programId || applyType === 'program' ? null : devOpsApi.project(projectId).countBranches(issueId));
      store.setBranch(res);
    })();
  }, [applyType, issueId, otherProject, outside, programId, projectId, store]);
  const branch = store.getBranch;
  const {
    totalCommit, commitUpdateTime, totalMergeRequest,
    mergeRequestStatus, mergeRequestUpdateTime,
  } = branch;
  const issue = store.getIssue;
  const {
    issueNum,
  } = issue;
  const renderBranchs = () => (
    <div>
      {
        branch.branchCount ? (
          <div>
            {
              [].length === 0 ? (
                <div style={{
                  borderBottom: '1px solid rgba(0, 0, 0, 0.08)', display: 'flex', padding: '8px 26px', alignItems: 'center', justifyContent: 'space-between', fontSize: '13px',
                }}
                >
                  <div style={{ display: 'inline-flex', justifyContent: 'space-between', flex: 1 }}>
                    <span
                      className="primary"
                      style={{ cursor: 'pointer' }}
                      role="none"
                      onClick={() => {
                        setCommitShow(true);
                      }}
                    >
                      {totalCommit || '0'}
                      {'提交'}
                    </span>
                  </div>
                  <div style={{ display: 'inline-flex', justifyContent: 'space-between' }}>
                    <span style={{ marginRight: 12, marginLeft: 63 }}>已更新</span>
                    <span style={{ width: 60, display: 'inline-block' }}>
                      {
                        commitUpdateTime ? (
                          <Popover
                            title="提交修改时间"
                            content={commitUpdateTime}
                            placement="topRight"
                          >
                            <TimeAgo
                              datetime={commitUpdateTime}
                              locale={Choerodon.getMessage('zh_CN', 'en')}
                            />
                          </Popover>
                        ) : ''
                      }
                    </span>
                  </div>
                </div>
              ) : null
            }
            {
              totalMergeRequest ? (
                <div style={{
                  borderBottom: '1px solid rgba(0, 0, 0, 0.08)', display: 'flex', padding: '8px 26px', alignItems: 'center', justifyContent: 'space-between', fontSize: '13px',
                }}
                >
                  <div style={{ display: 'inline-flex', justifyContent: 'space-between', flex: 1 }}>
                    <span
                      className="primary"
                      style={{ cursor: 'pointer' }}
                      role="none"
                      onClick={() => {
                        setMergeRequestShow(true);
                      }}
                    >
                      {totalMergeRequest}
                      {'合并请求'}
                    </span>
                    <span style={{
                      width: 36, height: 20, borderRadius: '2px', color: '#fff', background: '#4d90fe', textAlign: 'center',
                    }}
                    >
                      {['opened', 'merged', 'closed'].includes(mergeRequestStatus) ? STATUS_SHOW[mergeRequestStatus] : ''}
                    </span>
                  </div>
                  <div style={{ display: 'inline-flex', justifyContent: 'space-between' }}>
                    <span style={{ marginRight: 12, marginLeft: 63 }}>已更新</span>
                    <span style={{ width: 60, display: 'inline-block' }}>
                      {
                        mergeRequestUpdateTime ? (
                          <Popover
                            title="合并请求修改时间"
                            content={mergeRequestUpdateTime}
                            placement="topRight"
                          >
                            <TimeAgo
                              datetime={mergeRequestUpdateTime}
                              locale={Choerodon.getMessage('zh_CN', 'en')}
                            />
                          </Popover>
                        ) : ''
                      }
                    </span>
                  </div>
                </div>
              ) : null
            }
          </div>
        ) : (
          <div style={{
            borderBottom: '1px solid rgba(0, 0, 0, 0.08)', display: 'flex', padding: '8px 26px', alignItems: 'center', justifyContent: 'space-between', fontSize: '13px',
          }}
          >
            <span style={{ marginRight: 12 }}>暂无</span>
          </div>
        )
      }
    </div>
  );

  return (
    <div id="branch">
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          <FormattedMessage id="issue.branch" />
        </div>
        {!disabled && (
          <div className="c7n-title-right" style={{ marginLeft: '14px' }}>
            <Tooltip placement="topRight" title="关联分支" getPopupContainer={(triggerNode) => triggerNode.parentNode}>
              <Button style={{ padding: '0 6px' }} className="leftBtn" funcType="flat" onClick={() => store.setLinkBranchShow(true)}>
                <Icon type="add_branch icon" />
              </Button>
            </Tooltip>
            <Tooltip placement="topRight" title="创建分支" getPopupContainer={(triggerNode) => triggerNode.parentNode}>
              <Button style={{ padding: '0 6px' }} className="leftBtn" funcType="flat" onClick={() => store.setCreateBranchShow(true)}>
                <Icon type="playlist_add icon" />
              </Button>
            </Tooltip>
          </div>
        )}
      </div>
      {renderBranchs()}
      {
        commitShow ? (
          <Commits
            issueId={issueId}
            issueNum={issueNum}
            time={commitUpdateTime}
            onCancel={(needReload) => {
              setCommitShow(false);
              if (needReload) {
                reloadIssue();
              }
            }}
            visible={commitShow}
          />
        ) : null
      }
      {
        mergeRequestShow ? (
          <MergeRequest
            projectId={projectId}
            issueId={issueId}
            issueNum={issueNum}
            num={totalMergeRequest}
            onCancel={() => {
              setMergeRequestShow(false);
            }}
            visible={mergeRequestShow}
          />
        ) : null
      }
    </div>
  );
});

export default IssueBranch;
