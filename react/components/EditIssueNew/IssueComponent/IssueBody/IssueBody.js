/* eslint-disable no-nested-ternary */
import React, { Fragment, useContext } from 'react';
import { Tabs } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import FieldStar from './Field/FieldStar';
import IssueDetail from './IssueDetail';
import IssueDes from './IssueDes';
import IssueAttachment from './IssueAttachment';
import IssueDoc from './IssueDoc';
import IssueCommit from './IssueCommit';
import SplitStory from './SplitStory';
import IssueWorkLog from './IssueWorkLog';
import IssueLog from './IssueLog';
import SubTask from './SubTask';
import SubBug from './SubBug';
import IssueLink from './IssueLink';
import IssueBranch from './IssueBranch';
import IssueDropDown from '../IssueDropDown';
import IssuePIHistory from './IssuePIHistory';
import { FieldStoryPoint, FieldSummary } from './Field';
import CreateBranch from '../../../CreateBranch';
import LinkBranch from '../../../LinkBranch';
import DailyLog from '../../../DailyLog';
import IssueWSJF from './IssueWSJF';
import EditIssueContext from '../../stores';
import { InjectedComponent } from '../../injectComponent';
import './IssueBody.less';

const { TabPane } = Tabs;
// eslint-disable-next-line no-undef
const TestLink = Fragment;

function IssueBody(props) {
  const {
    prefixCls, disabled, store, isOnlyAgileProject, applyType,
  } = useContext(EditIssueContext);
  const issue = store.getIssue;
  const {
    issueId, issueNum, typeCode, issueTypeVO = {},
  } = issue;
  const { reloadIssue, otherProject, outside } = props;
  const createBranchShow = store.getCreateBranchShow;
  const { linkBranchShow } = store;
  const workLogShow = store.getWorkLogShow;

  return (
    <section className={`${prefixCls}-body`} id="scroll-area" style={{ position: 'relative' }}>
      <div style={{ paddingRight: 20 }}>
        <div style={{ display: 'flex', marginBottom: 10, alignItems: 'flex-start' }}>
          <FieldSummary
            {...props}
            showTitle={false}
            field={{ fieldCode: 'summary', fieldName: '概要' }}
          />
          <FieldStar {...props} />
          <div style={{ flexShrink: 0, marginLeft: 'auto', color: 'rgba(0, 0, 0, 0.65)' }}>
            {!disabled && (
              <IssueDropDown {...props} />
            )}
          </div>
        </div>
        {/* 故事点 */}
        <div className="line-start">
          {
            issueId && ['story', 'feature'].indexOf(typeCode) !== -1 ? (
              <div style={{ display: 'flex', marginRight: 25 }}>
                <FieldStoryPoint {...props} field={{ fieldCode: 'storyPoints', fieldName: '故事点' }} />
              </div>
            ) : null
          }
          {
            issueId && ['issue_epic', 'feature'].indexOf(typeCode) === -1 ? (
              <div style={{ display: 'flex' }}>
                <FieldStoryPoint {...props} field={{ fieldCode: 'remainingTime', fieldName: '剩余预估时间' }} />
              </div>
            ) : null
          }
        </div>
      </div>
      <Tabs defaultActiveKey="1">
        <TabPane tab="详情" key="1">
          <IssueDetail {...props} />
          <IssueDes {...props} />
          <IssueAttachment {...props} />
          {
            !outside && issueTypeVO.typeCode && issueTypeVO.typeCode === 'feature' && (
              <>
                <IssueWSJF {...props} />
                <InjectedComponent.PIAim {...props} />
              </>
            )
          }
          {!outside && !otherProject && issueTypeVO.typeCode && ['feature'].indexOf(issueTypeVO.typeCode) === -1
            ? <IssueDoc {...props} /> : ''}

          {issueTypeVO.typeCode && ['issue_epic', 'sub_task', 'feature'].indexOf(issueTypeVO.typeCode) === -1
            ? <SubTask {...props} /> : ''}

          {issueTypeVO.typeCode && ['story', 'task'].indexOf(issueTypeVO.typeCode) !== -1
            ? <SubBug {...props} /> : ''}
          {issueTypeVO.typeCode && ['feature', 'issue_epic'].indexOf(issueTypeVO.typeCode) === -1
            ? <TestLink {...props} /> : '' }
          {issueTypeVO.typeCode && ['feature', 'sub_task', 'issue_epic'].indexOf(issueTypeVO.typeCode) === -1
            ? <IssueLink {...props} /> : ''}
          {!outside && !otherProject && ['sub_task', 'issue_epic'].indexOf(issueTypeVO.typeCode) === -1 && <InjectedComponent.Backlog {...props} />}
        </TabPane>
        {
          issueTypeVO.typeCode && issueTypeVO.typeCode === 'feature'
            ? (
              <TabPane tab="拆分的Story" key="5">
                <SplitStory {...props} />
              </TabPane>
            ) : ''
        }
        <TabPane tab="评论" key="2">
          <IssueCommit {...props} />
        </TabPane>
        <TabPane tab="记录" key="3">
          {!disabled && issueTypeVO.typeCode === 'feature' && <IssuePIHistory {...props} />}
          {issueTypeVO.typeCode && ['feature', 'issue_epic'].indexOf(issueTypeVO.typeCode) === -1
            ? <IssueWorkLog {...props} /> : ''}
          <IssueLog {...props} />
        </TabPane>
        {applyType !== 'program' && !isOnlyAgileProject
          ? <TabPane tab="开发" key="4"><IssueBranch {...props} /></TabPane> : ''}
      </Tabs>
      {
        createBranchShow ? (
          <CreateBranch
            issueId={issueId}
            typeCode={typeCode}
            issueNum={issueNum}
            onOk={() => {
              store.setCreateBranchShow(false);
              if (reloadIssue) {
                reloadIssue(issueId);
              }
            }}
            onCancel={() => store.setCreateBranchShow(false)}
            visible={createBranchShow}
          />
        ) : null
      }
      {
        linkBranchShow ? (
          <LinkBranch
            issueId={issueId}
            typeCode={typeCode}
            issueNum={issueNum}
            onOk={() => {
              store.setLinkBranchShow(false);
              if (reloadIssue) {
                reloadIssue(issueId);
              }
            }}
            onCancel={() => store.setLinkBranchShow(false)}
            visible={linkBranchShow}
          />
        ) : null
      }
      {
        workLogShow ? (
          <DailyLog
            issueId={issueId}
            issueNum={issueNum}
            visible={workLogShow}
            onCancel={() => store.setWorkLogShow(false)}
            onOk={() => {
              reloadIssue(issueId);
              store.setWorkLogShow(false);
            }}
          />
        ) : null
      }
    </section>
  );
}

export default observer(IssueBody);
