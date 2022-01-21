/* eslint-disable no-restricted-globals */
import React, {
  useCallback, useRef, useEffect, useState,
} from 'react';
import { observer, Observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { Pagination, Tooltip, Button } from 'choerodon-ui/pro';
import { Draggable } from 'react-beautiful-dnd';
import { WindowScroller, List, AutoSizer } from 'react-virtualized';
import { usePersistFn } from 'ahooks';
import { inject } from 'mobx-react';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import Loading, { LoadingHiddenWrap } from '@/components/Loading';
import useDeepCompareEffect from '@/hooks/useDeepCompareEffect';
import { ISSUE_HEIGHT } from './constant';
import IssueItem from './IssueItem';
import NoneIssue from './NoneIssue';
import './IssueList.less';

function IssueList({
  data,
  sprintId,
  sprintData,
  openCreateIssueModal,
  snapshot: dropSnapshot,
  provided: dropProvided,
  AppState,
  sprintIndex,
}) {
  const listRef = useRef();

  const [createBtnToolTipHidden, setCreateBtnToolTipHidden] = useState(true);
  const [inNewUserGuideStepThree, setInNewUserGuideStepThree] = useState(false);

  useEffect(() => {
    if (
       AppState?.getUserWizardStatus[2]?.status === 'uncompleted'
    ) {
      setInNewUserGuideStepThree(true);
      setTimeout(() => {
        setCreateBtnToolTipHidden(false);
      }, 1000);
    } else {
      setInNewUserGuideStepThree(false);
      setCreateBtnToolTipHidden(true);
    }
  }, [AppState.getUserWizardStatus]);

  const shouldIncreaseHeight = usePersistFn((snapshot) => {
    const {
      isUsingPlaceholder,
      draggingOverWith,
      draggingFromThisWith,
    } = snapshot;
    const issueId = draggingFromThisWith || draggingOverWith;
    return (
      isUsingPlaceholder
      && !data.find((issue) => String(issue.issueId) === issueId)
    );
  });
  const handleExpandChange = usePersistFn((index) => {
    listRef.current.recomputeRowHeights(index);
  });
  useDeepCompareEffect(() => {
    if (listRef.current) {
      listRef.current.recomputeRowHeights();
    }
  }, [toJS(data)]);
  const renderIssueItem = useCallback(
    ({ index, style }) => {
      const issue = data[index];
      if (!issue) {
        return null;
      }
      return (
        <Draggable
          draggableId={String(issue.issueId)}
          index={index}
          key={issue.issueId}
        >
          {(provided) => (
            <IssueItem
              onExpandChange={handleExpandChange}
              provided={provided}
              issue={issue}
              style={{ margin: 0, ...style }}
              index={index}
              sprintId={sprintId}
            />
          )}
        </Draggable>
      );
    },
    [data, handleExpandChange, sprintId],
  );

  const getRowHeight = usePersistFn(({ index }) => {
    const issue = data[index];
    if (!issue) {
      return ISSUE_HEIGHT;
    }
    const isExpand = BacklogStore.isExpand(issue.issueId);
    return issue.children && isExpand
      ? ISSUE_HEIGHT * (issue.children.length + 1)
      : ISSUE_HEIGHT;
  });
  const { pagination, loading } = sprintData;
  const handlePaginationChange = usePersistFn((page, size) => {
    BacklogStore.updatePagination(sprintId, {
      // size变化，回到第一页
      page: pagination.size === size ? page : 1,
      size,
    });
    BacklogStore.refreshSprint(sprintId, false);
  });
  const rowCount = shouldIncreaseHeight(dropSnapshot)
    ? data.length + 1
    : data.length;

  const { total, page, size } = pagination;

  const toHelpDoc = () => {
    window.open(
      `${AppState?.getUserWizardStatus[2]?.helpDocs[1]}`,
      '_blank',
    );
  };

  const onHiddenBeforeChange = (hidden) => {
    if (inNewUserGuideStepThree && createBtnToolTipHidden === true && !hidden) {
      setCreateBtnToolTipHidden(hidden);
    }
  };

  const getCreatBtnTitle = () => {
    if (inNewUserGuideStepThree) {
      return (
        <div style={{ background: '#6E80F1 !important' }}>
          <div style={{ padding: 8 }}>
            工作项是项目的基石，是在成员在项目中通过各自的工作流进行的工作，一个问题可以代表一个史诗，一个故事，一个缺陷，一个任务等。问题能被单独分配给某个项目成员，并且设置跟踪的子任务。
          </div>
          <div style={{ textAlign: 'right' }}>
            <Button
              onClick={(e) => {
                e.stopPropagation();
                setCreateBtnToolTipHidden(true);
              }}
              style={{ color: '#fff', background: '#7E90F1' }}
            >
              忽略
            </Button>
            <Button
              onClick={toHelpDoc}
              style={{ color: '#5365EA', background: '#fff' }}
            >
              查看
            </Button>
          </div>
        </div>
      );
    }
    return '';
  };

  const getHidden = () => {
    const { modalOpened } = BacklogStore;
    if (modalOpened) {
      return true;
    }
    const allSprintData = BacklogStore.getSprintData;
    if (allSprintData.length <= 1) { // sprintData===1 还没创冲刺不显示
      return true;
    }
    if (sprintIndex !== 0) { // 第一个冲刺才显示新手提示
      return true;
    }
    if (data.length > 0) {
      return true;
    }
    return createBtnToolTipHidden;
  };

  const quickCreateIssueBtn = (
    <Tooltip
      popupClassName={
      inNewUserGuideStepThree ? 'c7n-pro-popup-create-issue-guide' : ''
    }
      hidden={getHidden()}
      onHiddenBeforeChange={onHiddenBeforeChange}
      title={getCreatBtnTitle}
      placement="bottomLeft"
    >
      <Button
        funcType="flat"
        icon="playlist_add"
        onClick={() => { setCreateBtnToolTipHidden(true); }}
      >
        创建工作项
      </Button>
    </Tooltip>
  );

  return (
    <div className="c7n-backlog-issue-list">
      <Observer>
        {() => (
          <Loading
            loading={loading}
            allowSelfLoading
            className="c7n-backlog-issue-list-loading"
          />
        )}
      </Observer>
      {data.length === 0 ? (
        <LoadingHiddenWrap>
          <NoneIssue type={sprintId === 0 ? 'backlog' : 'sprint'} />
        </LoadingHiddenWrap>
      ) : (
        <WindowScroller
          scrollElement={
            document.getElementsByClassName('c7n-backlog-content')[0]
          }
        >
          {({ height, scrollTop, registerChild }) => (
            <AutoSizer disableHeight>
              {({ width }) => (
                <div ref={(el) => registerChild(el)} style={{ width: '100%' }}>
                  <List
                    ref={listRef}
                    autoHeight
                    height={height}
                    rowCount={rowCount}
                    rowHeight={getRowHeight}
                    rowRenderer={renderIssueItem}
                    scrollTop={scrollTop}
                    width={width}
                    style={{
                      background: dropSnapshot.isDraggingOver
                        ? '#e9e9e9'
                        : 'inherit',
                      transition: 'background-color 0.2s ease',
                    }}
                  />
                </div>
              )}
            </AutoSizer>
          )}
        </WindowScroller>
      )}

      <div
        style={{
          padding: '10px 0px 10px 20px',
          borderBottom: '0.01rem solid var(--divider)',
          position: 'relative',
        }}
      >
        <QuickCreateIssue
          button={quickCreateIssueBtn}
          epicId={
          BacklogStore.getChosenEpic !== 'all'
          && BacklogStore.getChosenEpic !== 'unset'
            ? BacklogStore.getChosenEpic
            : undefined
          }
          versionIssueRelVOList={
              BacklogStore.getChosenVersion !== 'all'
              && BacklogStore.getChosenVersion !== 'unset'
                ? [
                  {
                    versionId: BacklogStore.getChosenVersion,
                  },
                ]
                : undefined
            }
          sprintId={sprintId}
          chosenFeatureId={
              BacklogStore.getChosenFeature !== 'all'
              && BacklogStore.getChosenFeature !== 'unset'
                ? BacklogStore.getChosenFeature
                : undefined
            }
          defaultAssignee={BacklogStore.filterSprintAssignUser.get(sprintId)}
          onCreate={(res) => {
            BacklogStore.handleCreateIssue(res, String(sprintId));
            BacklogStore.setShowIssueLoading(false);
            BacklogStore.refresh(false, false); // 更新侧边框
          }}
          cantCreateEvent={openCreateIssueModal}
          typeIdChange={(id) => {
            BacklogStore.setDefaultTypeId(id);
          }}
          summaryChange={(summary) => {
            BacklogStore.setDefaultSummary(summary);
          }}
          assigneeChange={(assigneeId, assignee) => {
            BacklogStore.setDefaultAssignee(assignee);
          }}
          setDefaultSprint={(value) => {
            BacklogStore.setDefaultSprint(value);
          }}
        />
      </div>

      {total > size ? (
        <div
          style={{
            display: 'flex',
            justifyContent: 'flex-end',
            margin: '10px 0',
          }}
        >
          <Pagination
            total={total}
            page={page}
            pageSize={size}
            onChange={handlePaginationChange}
            showSizeChangerLabel={false}
            pageSizeOptions={['10', '50', '100', '200', '300']}
            showTotal={(t, range) => `显示${range[0]}-${range[1]} 共 ${t}条`}
            showPager
            showQuickJumper
          />
        </div>
      ) : null}
    </div>
  );
}

export default inject('AppState')(observer(IssueList));
