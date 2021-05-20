import React, { useContext } from 'react';
import { Icon, Popover } from 'choerodon-ui';
import { useDetailContainerContext } from '@/components/detail-container/context';
import IssueNumber from './IssueNumber';
import IssueParentSummary from './IssueParentSummary';
import IssueParentTip from './IssueParentTip';
import IssueType from './IssueType';
import './IssueComponent.less';
import EditIssueContext from '../stores';
import './IssueHeader.less';

const IssueHeader = (props) => {
  const { AppState, store, prefixCls } = useContext(EditIssueContext);
  const { fullPage } = useDetailContainerContext();
  const {
    resetIssue, onCancel, reloadIssue, disabled,
  } = props;
  const issue = store.getIssue;
  const {
    parentIssueId, relateIssueId, typeCode, parentIssueSummary, parentRelateSummary, parentIssueDescription, parentRelateDescription,
    parentStarBeacon, relateStarBeacon,
  } = issue;
  return (
    <div className={`${prefixCls}-IssueHeader`}>
      <div className={`${prefixCls}-IssueHeader-top`}>
        <div style={{ display: 'flex', alignItems: 'center' }}>
          {
            parentIssueSummary || parentRelateSummary ? (
              <Popover
                overlayClassName={`${prefixCls}-IssueHeader-top-popover`}
                placement="leftTop"
                title="父任务详情"
                content={(
                  <IssueParentTip
                    parentSummary={parentIssueSummary || parentRelateSummary}
                    parentDescription={parentIssueDescription || parentRelateDescription}
                    parentStarBeacon={parentStarBeacon || relateStarBeacon}
                  />
                )}
                trigger="hover"
                getPopupContainer={((triggerNode) => triggerNode.parentNode)}
              >
                <div style={{
                  display: 'flex',
                  alignItems: 'center',
                  marginRight: !relateIssueId && !parentIssueId ? 15 : 0,
                  paddingLeft: 9,
                }}
                >
                  <IssueType {...props} />
                  <IssueParentSummary
                    parentIssueId={relateIssueId || parentIssueId}
                    resetIssue={resetIssue}
                    reloadIssue={reloadIssue}
                    parentSummary={parentIssueSummary || parentRelateSummary}
                    issue={issue}
                    disabled={disabled}
                  />
                </div>
              </Popover>
            ) : (
              <div style={{
                display: 'flex',
                alignItems: 'center',
                marginRight: !relateIssueId && !parentIssueId ? 15 : 0,
                paddingLeft: 12,
              }}
              >
                <IssueType {...props} />
                <IssueParentSummary
                  parentIssueId={relateIssueId || parentIssueId}
                  resetIssue={resetIssue}
                  reloadIssue={reloadIssue}
                  parentSummary={parentIssueSummary || parentRelateSummary}
                  issue={issue}
                  disabled={disabled}
                />
              </div>
            )
          }
          {/* 问题编号 */}
          <span className={`${prefixCls}-IssueHeader-top-number`}>
            <IssueNumber
              reloadIssue={reloadIssue}
              typeCode={typeCode}
              parentSummary={parentIssueSummary || parentRelateSummary}
              issue={issue}
              disabled={disabled}
            />
          </span>
        </div>

        {/* 隐藏 */}
        {!fullPage && (
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
        )}
      </div>
    </div>
  );
};

export default IssueHeader;
