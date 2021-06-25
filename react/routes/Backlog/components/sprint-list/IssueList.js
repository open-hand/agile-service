/* eslint-disable no-restricted-globals */
import React, { useCallback, useRef } from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { Pagination } from 'choerodon-ui/pro';
import { Droppable, Draggable } from 'react-beautiful-dnd';
import { WindowScroller, List, AutoSizer } from 'react-virtualized';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import { usePersistFn } from 'ahooks';
import Loading from '@/components/Loading';
import useDeepCompareEffect from '@/hooks/useDeepCompareEffect';
import { ISSUE_HEIGHT } from './constant';
import IssueItem from './IssueItem';
import NoneIssue from './NoneIssue';

function IssueList({ data, sprintId, sprintData }) {
  const listRef = useRef();
  const shouldIncreaseHeight = usePersistFn((snapshot) => {
    const { isUsingPlaceholder, draggingOverWith, draggingFromThisWith } = snapshot;
    const issueId = draggingFromThisWith || draggingOverWith;
    return isUsingPlaceholder && !data.find((issue) => String(issue.issueId) === issueId);
  });
  const handleExpandChange = usePersistFn((index) => {
    listRef.current.recomputeRowHeights(index);
  });
  useDeepCompareEffect(() => {
    if (listRef.current) {
      listRef.current.recomputeRowHeights();
    }
  }, [toJS(data)]);
  const renderIssueItem = useCallback(({ index, style }) => {
    const issue = data[index];
    if (!issue) {
      return null;
    }
    return (
      <Draggable draggableId={String(issue.issueId)} index={index} key={issue.issueId}>
        {(provided) => <IssueItem onExpandChange={handleExpandChange} provided={provided} issue={issue} style={{ margin: 0, ...style }} index={index} sprintId={sprintId} />}
      </Draggable>
    );
  }, [data, handleExpandChange, sprintId]);

  const handleOpenCreateIssue = useCallback(() => {
    BacklogStore.setNewIssueVisible(true);
  }, []);
  const getRowHeight = usePersistFn(({ index }) => {
    const issue = data[index];
    if (!issue) {
      return ISSUE_HEIGHT;
    }
    const isExpand = BacklogStore.isExpand(issue.issueId);
    return isExpand ? ISSUE_HEIGHT * 2 : ISSUE_HEIGHT;
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
  return (
    <Droppable
      droppableId={String(sprintId)}
      mode="virtual"
      isDropDisabled={BacklogStore.getIssueCantDrag}
      renderClone={(provided, snapshot, rubric) => (
        <IssueItem
          provided={provided}
          isDragging={snapshot.isDragging}
          issue={data[rubric.source.index]}
          sprintId={sprintId}
          style={{ margin: 0 }}
        />
      )}
    >
      {(provided, snapshot) => {
        const rowCount = shouldIncreaseHeight(snapshot)
          ? data.length + 1
          : data.length;
        return (
          <div
            ref={provided.innerRef}
          >
            {data.length === 0
              ? <NoneIssue type={sprintId === 0 ? 'backlog' : 'sprint'} />
              : (
                <WindowScroller scrollElement={document.getElementsByClassName('c7n-backlog-content')[0]}>
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
                              background: snapshot.isDraggingOver ? '#e9e9e9' : 'inherit',
                              transition: 'background-color 0.2s ease',
                            }}
                          />
                        </div>
                      )}
                    </AutoSizer>
                  )}
                </WindowScroller>
              )}
            <Loading loading={loading} />
            <div style={{ padding: '10px 0px 10px 20px', borderBottom: '0.01rem solid var(--divider)' }}>
              <QuickCreateIssue
                epicId={BacklogStore.getChosenEpic !== 'all' && BacklogStore.getChosenEpic !== 'unset' ? BacklogStore.getChosenEpic : undefined}
                versionIssueRelVOList={BacklogStore.getChosenVersion !== 'all' && BacklogStore.getChosenVersion !== 'unset' ? [
                  {
                    versionId: BacklogStore.getChosenVersion,
                  },
                ] : undefined}
                sprintId={sprintId}
                chosenFeatureId={BacklogStore.getChosenFeature !== 'all' && BacklogStore.getChosenFeature !== 'unset' ? BacklogStore.getChosenFeature : undefined}
                defaultAssignee={BacklogStore.filterSprintAssignUser.get(sprintId)}
                onCreate={(res) => {
                  BacklogStore.handleCreateIssue(res, String(sprintId));
                  BacklogStore.refresh(false, false); // 更新侧边框
                }}
                cantCreateEvent={handleOpenCreateIssue}
                typeIdChange={(id) => {
                  BacklogStore.setDefaultTypeId(id);
                }}
                summaryChange={(summary) => {
                  BacklogStore.setDefaultSummary(summary);
                }}
                assigneeChange={(assigneeId) => {
                  BacklogStore.setDefaultAssignee(assigneeId);
                }}
                setDefaultSprint={(value) => {
                  BacklogStore.setDefaultSprint(value);
                }}
              />
            </div>
            <div style={{ display: 'flex', justifyContent: 'flex-end', margin: '10px 0' }}>
              {pagination.total > pagination.size ? (
                <Pagination
                  total={pagination.total}
                  page={pagination.page}
                  pageSize={pagination.size}
                  onChange={handlePaginationChange}
                  showSizeChangerLabel={false}
                  showTotal={(total, range) => `显示${range[0]}-${range[1]} 共 ${total}条`}
                  showPager
                  showQuickJumper
                />
              ) : null}
            </div>
          </div>
        );
      }}
    </Droppable>
  );
}

export default observer(IssueList);
