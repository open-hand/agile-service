import React, { useRef, useCallback, useEffect } from 'react';
import {
  Button, Icon, Tooltip,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import MergeRequest from '@/components/MergeRequest';
import openLinkBranchModal from '@/components/LinkBranch/LinkBranchPro';
import openCreateBranchModal from '@/components/CreateBranch/CreateBranchPro';
import LinkedBranch from '../linked-branch';

const IssueBranch = observer(({
  store, disabled, projectId, reloadIssue, otherProject, outside, programId, applyType, issueId,
}) => {
  const commitsRef = useRef();
  const issue = store.getIssue;
  const {
    issueNum, typeCode,
  } = issue;
  const mergeRequestRef = useRef();
  const refresh = useCallback(() => {
    commitsRef.current?.query();
    mergeRequestRef.current?.loadMergeRequest();
  }, []);
  useEffect(() => {
    store.setRefreshBranch(refresh);
  }, [refresh, store]);

  return (
    <div id="branch">
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          关联分支
        </div>
        {!disabled && (
          <div className="c7n-title-right" style={{ marginLeft: '14px' }}>
            <Tooltip placement="topRight" title="关联分支">
              <Button onClick={() => openLinkBranchModal({ issueId, onOk: () => store.refreshBranch() })}>
                <Icon type="add_branch icon" />
              </Button>
            </Tooltip>
            <Tooltip placement="topRight" title="创建分支">
              <Button onClick={() => openCreateBranchModal({
                issueId, onOk: () => store.refreshBranch(), typeCode, defaultBranchSuffixName: issueNum,
              })}
              >
                <Icon type="playlist_add icon" />
              </Button>
            </Tooltip>
          </div>
        )}
      </div>
      <LinkedBranch ref={commitsRef} issueId={issueId} />
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          合并请求
        </div>
      </div>
      <MergeRequest
        projectId={projectId}
        ref={mergeRequestRef}
        issueId={issueId}
      />
    </div>
  );
});

export default IssueBranch;
