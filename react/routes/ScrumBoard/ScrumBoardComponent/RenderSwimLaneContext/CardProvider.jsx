import React from 'react';
import { observer } from 'mobx-react';
import { find } from 'lodash';
import { Draggable } from 'react-beautiful-dnd';
import { WindowScroller, List, AutoSizer } from 'react-virtualized';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import Card from './Card';
import { VIRTUAL_LIMIT } from './constant';

@observer
class CardProvider extends React.Component {
  shouldIncreaseHeight = () => {
    const {
      keyId, id, snapshot,
    } = this.props;
    const data = ScrumBoardStore.getSwimLaneData[keyId][id];
    const { isUsingPlaceholder, draggingOverWith, draggingFromThisWith } = snapshot;
    const draggableId = draggingFromThisWith || draggingOverWith;
    const issueId = draggableId ? draggableId.split('/')[1] : undefined;
    return isUsingPlaceholder && !find(data, { issueId });
  };

  renderIssueItem = ({ index, style }) => {
    const {
      keyId, id, completed, statusName, categoryCode,
    } = this.props;
    const data = ScrumBoardStore.getSwimLaneData[keyId][id];
    const issueObj = data[index];
    if (!issueObj) {
      return null;
    }
    const draggableId = `${keyId}/${issueObj.issueId}`;
    return (
      <Draggable draggableId={draggableId} index={index} key={draggableId}>
        {(provided) => (
          <Card
            provided={provided}
            key={`${issueObj.issueId}-${keyId}`}
            keyId={keyId}
            index={index}
            issue={issueObj}
            completed={completed}
            statusName={statusName}
            categoryCode={categoryCode}
            style={style}
          />
        )}
      </Draggable>

    );
  };

  render() {
    const {
      keyId, id, snapshot,
    } = this.props;
    const data = ScrumBoardStore.getSwimLaneData[keyId][id];
    const rowCount = this.shouldIncreaseHeight(snapshot)
      ? data.length + 1
      : data.length;
    return (
      data.length > VIRTUAL_LIMIT ? (
        <WindowScroller scrollElement={document.getElementsByClassName('c7n-scrumboard')[0]}>
          {({ height, scrollTop, registerChild }) => (
            <AutoSizer disableHeight>
              {({ width }) => (
                <div ref={(el) => registerChild(el)} style={{ width: '100%' }}>
                  <List
                    autoHeight
                    width={width}
                    height={height}
                    rowCount={rowCount}
                    rowHeight={120}
                    rowRenderer={this.renderIssueItem}
                    scrollTop={scrollTop}
                  />
                </div>
              )}
            </AutoSizer>
          )}
        </WindowScroller>
      ) : data.map((item, index) => this.renderIssueItem({ index }))
    );
  }
}
export default CardProvider;
