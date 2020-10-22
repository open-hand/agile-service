import React, { useContext } from 'react';
import { Icon } from 'choerodon-ui';
import IssueNumber from './IssueNumber';
import IssueType from './IssueType';
import './IssueComponent.less';
import EditIssueContext from '../stores';
import './IssueHeader.less';

const IssueHeader = (props) => {
  const { AppState, store, prefixCls } = useContext(EditIssueContext);
  const {
    resetIssue, backUrl, onCancel, reloadIssue, disabled,
  } = props;
  const urlParams = AppState.currentMenuType;
  const issue = store.getIssue;
  const {
    parentIssueId, relateIssueId, typeCode, parentIssueSummary, parentRelateSummary, parentIssueDescription, parentRelateDescription,
  } = issue;
  return (
    <div className={`${prefixCls}-IssueHeader`}>
      <div className={`${prefixCls}-IssueHeader-top`}>
        <IssueType {...props} />
        {/* 问题编号 */}
        <span style={{ marginLeft: 15 }} className={`${prefixCls}-IssueHeader-top-number`}>
          <IssueNumber
            parentIssueId={relateIssueId || parentIssueId}
            resetIssue={resetIssue}
            reloadIssue={reloadIssue}
            urlParams={urlParams}
            backUrl={backUrl}
            typeCode={typeCode}
            // parentIssueNum={parentIssueNum || relateIssueNum}
            parentSummary={parentIssueSummary || parentRelateSummary}
            parentDescription={parentIssueDescription || parentRelateDescription}
            issue={issue}
            disabled={disabled}
          />
        </span>
        {/* 隐藏 */}
        <div
          className={`${prefixCls}-IssueHeader-btn`}
          role="none"
          onClick={() => {
            onCancel();
          }}
        >
          <Icon type="last_page" style={{ fontSize: '18px', fontWeight: '500' }} />
          <span>隐藏详情</span>
        </div>
      </div>
    </div>
  );
};

export default IssueHeader;
