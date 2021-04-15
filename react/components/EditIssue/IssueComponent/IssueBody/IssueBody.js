/* eslint-disable no-nested-ternary */
import React, { Fragment, useContext, useRef } from 'react';
import { Tabs } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import { mount } from '@choerodon/inject';
import { useDetailContainerContext } from '@/components/detail-container/context';
import useHasDevops from '@/hooks/useHasDevops';
import useHasTest from '@/hooks/useHasTest';
import FieldStar from './Field/FieldStar';
import IssueDetail from './IssueDetail';
import IssueDes from './IssueDes';
import IssueAttachment from './IssueAttachment';
import IssueDoc from './IssueDoc';
import Comments from '../commentsWithReply';
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
import IssueUI from './Issue-UI';

const { TabPane } = Tabs;

function IssueBody(props) {
  const {
    prefixCls, disabled, store, applyType,
  } = useContext(EditIssueContext);
  const { match } = useDetailContainerContext();
  const issue = store.getIssue;
  const {
    issueId, issueNum, typeCode, issueTypeVO = {},
  } = issue;
  const { reloadIssue, otherProject, outside } = props;
  const hasDevops = useHasDevops();
  const createBranchShow = store.getCreateBranchShow;
  const { linkBranchShow } = store;
  const workLogShow = store.getWorkLogShow;
  const hasTest = useHasTest();
  const testLinkStoreRef = useRef();
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
              <IssueDropDown {...props} testLinkStoreRef={testLinkStoreRef} />
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
      <Tabs
        activeKey={store.tab}
        onChange={(activeKey) => {
          store.setTab(activeKey);
          match.props.tab = activeKey;
        }}
      >
        <TabPane tab="详情" key="detail">
          <IssueDetail {...props} />
          <IssueDes {...props} />
          <IssueAttachment {...props} />
          {issueTypeVO.typeCode && ['issue_epic', 'feature'].indexOf(issueTypeVO.typeCode) === -1
            ? <IssueUI {...props} /> : ''}
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
          {hasTest && issueTypeVO.typeCode && ['feature', 'issue_epic'].indexOf(issueTypeVO.typeCode) === -1
            ? mount('testmanager:IssueLinkedTestCase', {
              testLinkStoreRef,
              noCreateLink: true,
              ...props,
            }) : ''}
          {issueTypeVO.typeCode && ['feature', 'sub_task', 'issue_epic'].indexOf(issueTypeVO.typeCode) === -1
            ? <IssueLink {...props} /> : ''}
          {!outside && ['sub_task', 'issue_epic'].indexOf(issueTypeVO.typeCode) === -1 && <InjectedComponent.Backlog {...props} />}
        </TabPane>
        {
          issueTypeVO.typeCode && issueTypeVO.typeCode === 'feature'
            ? (
              <TabPane tab="拆分的Story" key="split_story">
                <SplitStory {...props} />
              </TabPane>
            ) : ''
        }
        <TabPane tab="评论" key="comment">
          <Comments {...props} />
        </TabPane>
        <TabPane tab="记录" key="record">
          {!disabled && issueTypeVO.typeCode === 'feature' && <IssuePIHistory {...props} />}
          {issueTypeVO.typeCode && ['feature', 'issue_epic'].indexOf(issueTypeVO.typeCode) === -1
            ? <IssueWorkLog {...props} /> : ''}
          <IssueLog {...props} />
        </TabPane>
        {applyType !== 'program' && hasDevops
          ? <TabPane tab="开发" key="development"><IssueBranch {...props} /></TabPane> : ''}
      </Tabs>
      {
        createBranchShow ? (
          <CreateBranch
            issueId={issueId}
            typeCode={typeCode}
            issueNum={issueNum}
            onOk={() => {
              store.setCreateBranchShow(false);
              store.refreshBranch();
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
              store.refreshBranch();
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
