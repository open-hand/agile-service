import React, { useContext } from 'react';
import { Icon, Popover } from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
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
    resetIssue, onCancel, reloadIssue, disabled, otherProject, outside,
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
          {/* 工作项编号 */}
          <span className={`${prefixCls}-IssueHeader-top-number`}>
            <IssueNumber
              reloadIssue={reloadIssue}
              typeCode={typeCode}
              parentSummary={parentIssueSummary || parentRelateSummary}
              issue={issue}
              disabled={disabled}
              otherProject={otherProject}
              outside={outside}
            />
          </span>
        </div>

        {/* 隐藏 */}
        {!fullPage && (
          <Button
            funcType="flat"
            icon="last_page"
            onClick={() => {
              onCancel();
            }}
          >
            隐藏详情
          </Button>
        )}
      </div>
    </div>
  );
};

export default IssueHeader;
