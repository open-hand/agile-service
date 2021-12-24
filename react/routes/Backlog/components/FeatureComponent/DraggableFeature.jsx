import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Menu, Icon, Dropdown } from 'choerodon-ui';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import { featureApi } from '@/api';


@observer
class DraggableFeature extends Component {
  constructor(props) {
    super(props);
    this.state = {
      expand: false,
      editName: false,
      colorVisible: false,
    };
  }

  /**
   *menu的点击事件
   *
   * @param {*} e
   * @memberof FeatureItem
   */
  clickMenu = (e) => {
    const { item } = this.props;
    e.domEvent?.stopPropagation();
    if (e.key === '1') {
      this.setState({
        editName: true,
      });
    }
    if (e.key === '2') {
      BacklogStore.setClickIssueDetail(item);
    }
  }

  handleClickDetail=(e) => {
    const { item } = this.props;
    e.stopPropagation();
    BacklogStore.setClickIssueDetail(item);
  }

  /**
   *每个feature 右侧下拉选择项的menu
   *
   * @returns
   * @memberof FeatureItem
   */
  getmenu = () => {
    const { item, refresh } = this.props;
    return (
      <Menu onClick={this.clickMenu.bind(this)}>
        <div style={{ padding: '5px 12px', marginTop: '10px' }}>
          颜色
          <div className="c7n-backlog-epicColor">
            {BacklogStore.getColorLookupValue.map(color => (
              <div
                key={color.name}
                style={{ background: color.name }}
                className="c7n-backlog-epicColorItem"
                role="none"
                onClick={(e) => {
                  e.stopPropagation();
                  this.toggleClickColor(false);
                  const inputData = {
                    colorCode: color.valueCode,
                    issueId: item.issueId,
                    objectVersionNumber: item.objectVersionNumber,
                  };
                  BacklogStore.updateFeature({ ...item, color: color.name });
                  featureApi.updateColor(inputData).then((res) => {
                    BacklogStore.updateFeature(res);
                    refresh();
                  }).catch((error) => {
                    BacklogStore.updateFeature(item);
                  });
                }}
              />
            ))}
          </div>
        </div>
      </Menu>
    );
  }

  toggleExpand = (e) => {
    e.stopPropagation();
    const { expand } = this.state;
    this.setState({
      expand: !expand,
    });
  }

  toggleClickColor = (visible) => {
    this.setState({
      colorVisible: visible,
    });
  };

  render() {
    const {
      draggableProvided, item,
    } = this.props;
    const { expand, editName, colorVisible } = this.state;
    return (
      <div
        ref={draggableProvided.innerRef}
        {...draggableProvided.draggableProps}
        {...draggableProvided.dragHandleProps}
        className={classnames('c7n-backlog-epicItems', {
          onClickFeature: BacklogStore.getChosenFeature === item.issueId,
        })}
        role="none"
      >
        <div
          className="c7n-backlog-epicItemTitle"
        >
          <Icon
            type={expand ? 'baseline-arrow_drop_down' : 'baseline-arrow_right'}
            role="none"
            onClick={this.toggleExpand}
          />
      
          <div style={{ width: '100%' }}>
            <div className="c7n-backlog-epicItemsHead">              
              <p>{item.summary}</p>
              <Dropdown
                onClick={e => e.stopPropagation()}
                overlay={this.getmenu()}
                trigger={['click']}
                visible={colorVisible}
                onVisibleChange={this.toggleClickColor}
              >
                <Icon
                  style={{
                    width: 12,
                    height: 12,
                    background: item.color || BacklogStore.randomFeatureColor[item.summary],
                    color: 'white',
                    display: 'flex',
                    justifyContent: 'center',
                    alignItems: 'center',
                    borderRadius: 2,
                  }}
                  type="arrow_drop_down"
                />
              </Dropdown>
            </div>
         
            <div
              className="c7n-backlog-epicItemProgress"
            >
              <div
                className="c7n-backlog-epicItemDone"
                style={{
                  flex: item.successStoryPoints,
                }}
              />
              <div
                className="c7n-backlog-epicItemTodo"
                style={{
                  flex: item.totalStoryPoints ? item.totalStoryPoints - item.successStoryPoints : 1,
                }}
              />
            </div>
          </div>
        </div>
        {expand ? (
          <div style={{ paddingLeft: 12 }}>
            {/* <p className="c7n-backlog-epicItemDes">
              {_.isNull(item.summary) ? '没有描述' : item.summary}
            </p> */}
            <div className="c7n-backlog-epicItemDetail">
              <span>计数详情</span>
              <div
                role="none"
                onClick={this.handleClickDetail}
                className="c7n-backlog-epicItemDetail-more"
              >
                查看详情
              </div>
            </div>
            <div className="c7n-backlog-epicItemParams">
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">总故事数量</p>
                <p className="c7n-backlog-epicItemNotStoryPoint">{item.storyCount}</p>
              </div>
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">已完成故事数</p>
                <p className="c7n-backlog-epicItemNotStoryPoint">{item.storyCompletedCount}</p>
              </div>              
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">总故事点数</p>
                <p
                  className="c7n-backlog-epicItemParamValue"
                  style={{ minWidth: 31, color: 'var(--text-color3)' }}
                >
                  {item.totalStoryPoints}
                </p>
              </div>
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">已完成故事点数</p>
                <p
                  className="c7n-backlog-epicItemParamValue"
                  style={{ minWidth: 31, color: 'var(--text-color3)' }}
                >
                  {item.successStoryPoints}
                </p>
              </div>
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">未预估故事数</p>
                <p className="c7n-backlog-epicItemNotStoryPoint">{item.unEstimateStoryCount}</p>
              </div>
            </div>
          </div>
        ) : ''}
      </div>
    );
  }
}

export default DraggableFeature;
