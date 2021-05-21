/* eslint-disable no-restricted-globals */
import React, { useCallback, useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import { Droppable, Draggable } from 'react-beautiful-dnd';
import { WindowScroller, List, AutoSizer } from 'react-virtualized';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import QuickCreateIssue from '@/components/QuickCreateIssue';
import IssueItem from './IssueItem';
import NoneIssue from './NoneIssue';

function IssueList({ data, sprintId }) {
  const issueMap = useMemo(() => new Map(data.map((issue) => [String(issue.issueId), true])), [data.length]);
  const shouldIncreaseHeight = useCallback((snapshot) => {
    const { isUsingPlaceholder, draggingOverWith, draggingFromThisWith } = snapshot;
    const issueId = draggingFromThisWith || draggingOverWith;
    return isUsingPlaceholder && !issueMap.has(issueId);
  }, [issueMap]);
  const renderIssueItem = useCallback(({ index, style }) => {
    const issue = data[index];
    if (!issue) {
      return null;
    }
    return (
      <Draggable draggableId={String(issue.issueId)} index={index} key={issue.issueId}>
        {(provided) => <IssueItem provided={provided} issue={issue} style={{ margin: 0, ...style }} index={index} sprintId={sprintId} />}
      </Draggable>
    );
  }, [data]);

  const handleOpenCreateIssue = useCallback(() => {
    BacklogStore.setNewIssueVisible(true);
  }, []);

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
            {rowCount === 0 || (rowCount === 1 && snapshot.isUsingPlaceholder) > 0
              ? <NoneIssue type={sprintId === 0 ? 'backlog' : 'sprint'} />
              : (
                <WindowScroller scrollElement={document.getElementsByClassName('c7n-backlog-content')[0]}>
                  {({ height, scrollTop, registerChild }) => (
                    <AutoSizer disableHeight>
                      {({ width }) => (
                        <div ref={(el) => registerChild(el)} style={{ width: '100%' }}>
                          <List
                            autoHeight
                            height={height}
                            rowCount={rowCount}
                            rowHeight={48}
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
          </div>
        );
      }}
    </Droppable>
  );
}

export default observer(IssueList);
