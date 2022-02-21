/* eslint-disable no-nested-ternary */
import React, {
  Fragment, useContext, useRef, useState, useEffect,
} from 'react';
import { Tabs } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import { has as hasInject, mount } from '@choerodon/inject';
import map from 'lodash/map';
import { useDetailContainerContext } from '@/components/detail-container/context';
import useHasDevops from '@/hooks/useHasDevops';
import useHasTest from '@/hooks/useHasTest';
import FieldStar from './Field/FieldStar';
import IssueDetail from './IssueDetail';
import IssueDes from './IssueDes';
import IssueAttachment from './IssueAttachment';
import IssueDoc from './IssueDoc';
import Comments from '../commentsWithReply';
import IssueSplit from './issue-split';
import IssueWorkLog from './IssueWorkLog';
import IssueLog from './IssueLog';
import SubTask from './SubTask';
import SubBug from './SubBug';
import IssueLink from './IssueLink';
import IssueBranch from './IssueBranch';
import IssueDropDown from '../IssueDropDown';
import IssuePIHistory from './IssuePIHistory';
import { FieldStoryPoint, FieldSummary } from './Field';
import IssueWSJF from './IssueWSJF';
import EditIssueContext from '../../stores';
import { InjectedComponent } from '../../injectComponent';
import './IssueBody.less';
import IssueUI from './Issue-UI';

import { featureApi } from '@/api';
import { getProjectId } from '@/utils/common';
import { DEPENDENCY_TAB, SUB_ISSUE } from '../../../../constants/WATERFALL_INJECT';
import { WATERFALL_TYPE_CODES } from '../../../../constants/TYPE_CODE';

const { TabPane } = Tabs;

function IssueBody(props) {
  const {
    prefixCls, disabled, store, applyType,
  } = useContext(EditIssueContext);
  const { match } = useDetailContainerContext();
  const { comments } = store;
  const issue = store.getIssue;
  const {
    issueId, issueNum, typeCode, issueTypeVO = {}, projectId, projectVO,
  } = issue;
  const { otherProject, outside } = props;
  const hasDevops = useHasDevops(projectVO?.categories ? map(projectVO.categories, 'code') : null);
  const hasTest = useHasTest();
  const testLinkStoreRef = useRef();

  const [splitStoryData, setSplitStoryData] = useState();

  useEffect(() => {
    const loadData = async () => {
      let Data;
      if (outside) {
        Data = await await featureApi.getSplitStoryOutside(issueId, projectId);
      } else {
        Data = store.projectId?.toString() !== projectId.toString() ? await featureApi.getSubProjectSplitStory(issueId, projectId) : await featureApi.getSplitStory(issueId);
      }
      setSplitStoryData(Data);
    };

    if (issueTypeVO.typeCode && issueTypeVO.typeCode === 'feature') {
      loadData();
    }
  }, [issueId, issueTypeVO.typeCode, outside, projectId, store.projectId]);

  return (
    <section className={`${prefixCls}-body`} id="scroll-area" style={{ position: 'relative' }}>
      <div style={{ paddingRight: 20, marginBottom: 10 }}>
        <div style={{ display: 'flex', marginBottom: 10, alignItems: 'flex-start' }}>
          <FieldSummary
            {...props}
            showTitle={false}
            field={{ fieldCode: 'summary', fieldName: '概要' }}
          />
          <FieldStar {...props} />
          <div style={{ flexShrink: 0, marginLeft: 'auto', color: 'var(--text-color3)' }}>
            {!disabled && (
              <IssueDropDown {...props} testLinkStoreRef={testLinkStoreRef} />
            )}
          </div>
        </div>
        {/* 故事点 */}
        <div className="line-start" style={{ flexWrap: 'wrap' }}>
          {
            issueId && ['story', 'feature'].indexOf(typeCode) !== -1 ? (
              <div style={{ display: 'flex', marginRight: 15 }}>
                <FieldStoryPoint {...props} field={{ fieldCode: 'storyPoints', fieldName: '故事点' }} />
              </div>
            ) : null
          }
          {
            issueId && ['issue_epic', 'feature'].indexOf(typeCode) === -1 && (
              <div style={{ display: 'flex', marginRight: 15 }}>
                <FieldStoryPoint {...props} field={{ fieldCode: 'estimateTime', fieldName: '原始预估时间' }} />
              </div>
            )
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
          {issueTypeVO.typeCode && ['story', 'task', 'bug'].indexOf(issueTypeVO.typeCode) !== -1
            ? <IssueLink {...props} /> : ''}
          {!outside && ['sub_task', 'issue_epic', ...WATERFALL_TYPE_CODES].indexOf(issueTypeVO.typeCode) === -1 && <InjectedComponent.Backlog {...props} />}
        </TabPane>
        {issueTypeVO.typeCode && WATERFALL_TYPE_CODES.includes(issueTypeVO.typeCode) && hasInject(DEPENDENCY_TAB)
          ? <TabPane tab="依赖与关联">{mount(DEPENDENCY_TAB, { ...props, issueTypeCode: issueTypeVO.typeCode })}</TabPane> : null}
        {
          issueTypeVO.typeCode && issueTypeVO.typeCode === 'feature'
            ? (
              <TabPane tab={`拆分的工作项${splitStoryData?.storyList ? `(${(splitStoryData?.storyList?.length || 0) > 99 ? '99+' : (splitStoryData?.storyList?.length || 0)})` : ''}`} key="split_story">
                <IssueSplit {...props} issueData={splitStoryData} />
              </TabPane>
            ) : ''
        }
        <TabPane tab={`评论${comments ? `(${(comments?.totalElements || 0) > 99 ? '99+' : (comments?.totalElements || 0)})` : ''}`} key="comment">
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

    </section>
  );
}

export default observer(IssueBody);
