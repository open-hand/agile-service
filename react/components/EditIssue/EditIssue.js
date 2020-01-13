/* eslint-disable react/sort-comp */
import React, {
  useContext, useState, useEffect, useImperativeHandle, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import { stores, axios } from '@choerodon/boot';
import { Spin } from 'choerodon-ui';
import { throttle } from 'lodash';
import './EditIssue.less';
import {
  loadBranchs, loadDatalogs, loadLinkIssues,
  loadIssue, loadWorklogs, loadDocs, getFieldAndValue, loadIssueTypes,
} from '../../api/NewIssueApi';
import {
  loadDatalogs as loadDatalogsProgram,
  loadIssue as loadIssueProgram,
  getFieldAndValue as getFieldAndValueProgram,
} from '../../api/QueryProgramApi';
import RelateStory from '../RelateStory';
import CopyIssue from '../CopyIssue';
import ResizeAble from '../ResizeAble';
import TransformSubIssue from '../TransformSubIssue';
import TransformFromSubIssue from '../TransformFromSubIssue';
import ChangeParent from '../ChangeParent';
import IssueHeader from './IssueComponent/IssueHeader';
import IssueBody from './IssueComponent/IssueBody/IssueBody';
import EditIssueContext from './stores';
import IsInProgramStore from '../../stores/common/program/IsInProgramStore';
// 项目加入群之后，不关联自己的史诗和特性，只能关联项目群的，不能改关联的史诗
const { AppState } = stores;

let loginUserId;
let hasPermission;
const defaultProps = {
  applyType: 'agile',
};

const EditIssue = observer(() => {
  const [issueLoading, setIssueLoading] = useState(false);
  // 侧滑详情高度
  const [isHasBanner, setIsHasBanner] = useState(false);
  const {
    onCurrentClicked, // 设置当前加载的问题详情信息
    store,
    forwardedRef,
    issueId: currentIssueId,
    applyType,
    programId,
    onUpdate,
    onIssueCopy,
    backUrl,
    onCancel,
    style,
    onDeleteIssue,
    onDeleteSubIssue,
    disabled,
    prefixCls,
    FieldVersionRef,
    FieldFixVersionRef,
    intlPrefix,
    issueStore,
    HeaderStore,
    isFullScreen,
    onChangeWidth,
  } = useContext(EditIssueContext);
  const container = useRef();
  const idRef = useRef();
  const loadIssueDetail = (paramIssueId) => {
    if (FieldVersionRef.current) {
      FieldVersionRef.current.loadIssueVersions();
    }
    if (FieldFixVersionRef.current) {
      FieldFixVersionRef.current.loadIssueVersions();
    }
    const id = paramIssueId || currentIssueId;
    idRef.current = id;
    setIssueLoading(true);
    (programId ? loadIssueProgram(id, programId) : loadIssue(id)).then((res) => {
      if (idRef.current !== id) {
        return;
      }
      // 刷新MOBX中详情信息，防止在列表界面再次点击父问题时无法跳转
      if (onCurrentClicked) {
        onCurrentClicked(res);
      }

      const param = {
        schemeCode: 'agile_issue',
        context: res.typeCode,
        pageCode: 'agile_issue_edit',
      };
      (programId ? getFieldAndValueProgram(id, param, programId) : getFieldAndValue(id, param)).then((fields) => {
        setIssueLoading(false);
        store.setIssueFields(res, fields);
      });
      if (issueStore) {
        issueStore.setSelectedIssue(res);
      }
    });
    if (programId || applyType === 'program') {
      axios.all([
        loadDocs(id),
        programId ? loadDatalogsProgram(id, programId) : loadDatalogs(id),
      ]).then(axios.spread((doc, dataLogs) => {
        if (idRef.current !== id) {
          return;
        }
        store.initIssueAttribute(doc, [], dataLogs, [], {}, []);
      }));
    } else {
      axios.all([
        loadDocs(id),
        loadWorklogs(id),
        loadDatalogs(id),
        loadLinkIssues(id),
        loadBranchs(id),
        // getTestExecute(id),
      ])
        .then(axios.spread((doc, workLogs, dataLogs, linkIssues, branches) => {          
          if (idRef.current !== id) {
            return;
          }
          store.initIssueAttribute(doc, workLogs, dataLogs, linkIssues, branches, []);
        }));
    }
  };


  const setQuery = (width = container.current.clientWidth) => {
    if (width <= 600) {
      container.current.setAttribute('max-width', '600px');
    } else {
      container.current.removeAttribute('max-width');
    }
  };
  useEffect(() => {    
    loadIssueDetail(currentIssueId);
    if (!programId) {
      axios.all([
        axios.get('/base/v1/users/self'),
        axios.post('/base/v1/permissions/checkPermission', [{
          code: 'agile-service.project-info.updateProjectInfo',
          organizationId: AppState.currentMenuType.organizationId,
          projectId: AppState.currentMenuType.id,
          resourceType: 'project',
        }, {
          code: 'agile-service.notice.queryByProjectId',
          organizationId: AppState.currentMenuType.organizationId,
          projectId: AppState.currentMenuType.id,
          resourceType: 'project',
        }]),
        loadIssueTypes(applyType),
      ])
        .then(axios.spread((users, permission, issueTypes) => {
          loginUserId = users.id;
          hasPermission = permission[0].approve || permission[1].approve;
          store.setIssueTypes(issueTypes);
        }));
    }
    setQuery();
  }, [currentIssueId]);

  const handleCopyIssue = (issue) => {
    store.setCopyIssueShow(false);
    loadIssueDetail(issue.issueId);
    if (onIssueCopy) {
      onIssueCopy(issue);
    } else if (onUpdate) {
      onUpdate();
    }
  };

  const handleRelateStory = () => {
    store.setRelateStoryShow(false);
    if (onUpdate) {
      onUpdate();
    }
    loadIssueDetail();
  };

  const handleTransformSubIssue = () => {
    store.setTransformSubIssueShow(false);
    if (onUpdate) {
      onUpdate();
    }
    loadIssueDetail();
  };

  const handleTransformFromSubIssue = () => {
    store.setTransformFromSubIssueShow(false);
    if (onUpdate) {
      onUpdate();
    }
    loadIssueDetail();
  };

  const handleResizeEnd = ({ width }) => {
    localStorage.setItem('agile.EditIssue.width', `${width}px`);
  };

  useImperativeHandle(forwardedRef, () => ({
    loadIssueDetail,
  }));

  const handleResize = throttle(({ width }) => {
    setQuery(width);
    if (onChangeWidth) {
      onChangeWidth(width);// 设置宽度
    }
  }, 150);


  const issue = store.getIssue;
  const {
    issueId, issueNum, summary,
    assigneeId, objectVersionNumber, createdBy, typeCode,
  } = issue;
  const linkIssues = store.getLinkIssues;

  const {
    getChangeParentShow: changeParentShow,
    getAssigneeShow: assigneeShow,
    getCopyIssueShow: copyIssueShow,
    getTransformSubIssueShow: transformSubIssueShow,
    getTransformFromSubIssueShow: transformFromSubIssueShow,
    getRelateStoryShow: relateStoryShow,
  } = store;
  const rightDisabled = disabled || (IsInProgramStore.isInProgram && typeCode === 'issue_epic');
  const HasPermission = (hasPermission || createdBy === AppState.userInfo.id);
  return (
    <div style={{
      position: 'fixed',
      right: 0,
      // eslint-disable-next-line no-nested-ternary
      top: isFullScreen ? 0 : HeaderStore.announcementClosed ? 50 : 100,
      bottom: 0,
      // height: 'calc(100vh - 50px)',
      zIndex: 101,
      overflow: 'hidden',
    }}
    >
      <ResizeAble
        modes={['left']}
        size={{
          maxWidth: window.innerWidth * 0.6,
          minWidth: 440,
        }}
        defaultSize={{
          width: localStorage.getItem('agile.EditIssue.width') || 605,
          height: '100%',
        }}
        onResizeEnd={handleResizeEnd}
        onResize={handleResize}
      >
        <div className={`${prefixCls}`} style={style} ref={container}>
          <div className={`${prefixCls}-divider`} />
          {
            issueLoading ? (
              <div
                style={{
                  position: 'absolute',
                  top: 0,
                  bottom: 0,
                  left: 0,
                  right: 0,
                  background: 'rgba(255, 255, 255, 0.65)',
                  zIndex: 9999,
                  display: 'flex',
                  justifyContent: 'center',
                  alignItems: 'center',
                }}
              >
                <Spin />
              </div>
            ) : null
          }
          <div className="c7n-content">
            <IssueHeader
              disabled={rightDisabled}
              store={store}
              reloadIssue={loadIssueDetail}
              backUrl={backUrl}
              onCancel={onCancel}
              loginUserId={loginUserId}
              hasPermission={HasPermission}
              onDeleteIssue={onDeleteIssue}
              onUpdate={onUpdate}
            />
            <IssueBody
              key={issueId}
              disabled={rightDisabled}
              store={store}
              reloadIssue={loadIssueDetail}
              onUpdate={onUpdate}
              onDeleteSubIssue={onDeleteSubIssue}
              loginUserId={loginUserId}
              hasPermission={hasPermission}
              applyType={applyType}
              onDeleteIssue={onDeleteIssue}
              parentSummary={summary}
            />
          </div>
          {
            copyIssueShow ? (
              <CopyIssue
                issueId={issueId}
                issueNum={issueNum}
                issue={issue}
                issueLink={linkIssues}
                issueSummary={summary}
                visible={copyIssueShow}
                onCancel={() => store.setCopyIssueShow(false)}
                onOk={handleCopyIssue.bind(this)}
                applyType={applyType}
              />
            ) : null
          }
          {
            relateStoryShow ? (
              <RelateStory
                issue={issue}
                visible={relateStoryShow}
                onCancel={() => store.setRelateStoryShow(false)}
                onOk={handleRelateStory.bind(this)}
              />
            ) : null
          }
          {
            transformSubIssueShow ? (
              <TransformSubIssue
                visible={transformSubIssueShow}
                issueId={issueId}
                issueNum={issueNum}
                ovn={objectVersionNumber}
                onCancel={() => store.setTransformSubIssueShow(false)}
                onOk={handleTransformSubIssue.bind(this)}
                store={store}
              />
            ) : null
          }
          {
            transformFromSubIssueShow ? (
              <TransformFromSubIssue
                visible={transformFromSubIssueShow}
                issueId={issueId}
                issueNum={issueNum}
                ovn={objectVersionNumber}
                onCancel={() => store.setTransformFromSubIssueShow(false)}
                onOk={handleTransformFromSubIssue.bind(this)}
                store={store}
              />
            ) : null
          }
          {
            changeParentShow ? (
              <ChangeParent
                issueId={issueId}
                issueNum={issueNum}
                visible={changeParentShow}
                objectVersionNumber={objectVersionNumber}
                onOk={() => {
                  store.setChangeParentShow(false);
                  loadIssueDetail(issueId);
                }}
                onCancel={() => {
                  store.setChangeParentShow(false);
                }}
              />
            ) : null
          }
        </div>
      </ResizeAble>
    </div>
  );
});
EditIssue.defaultProps = defaultProps;
export default EditIssue;
