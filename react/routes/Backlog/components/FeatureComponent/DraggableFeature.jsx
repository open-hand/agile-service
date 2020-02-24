import React, { Component } from 'react';
import { stores, axios, Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Menu, Icon } from 'choerodon-ui';
import BacklogStore from '@/stores/project/backlog/BacklogStore';

const { AppState } = stores;

@observer
class DraggableFeature extends Component {
  constructor(props) {
    super(props);
    this.state = {
      expand: false,
      editName: false,
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
    e.domEvent.stopPropagation();
    if (e.key === '1') {
      this.setState({
        editName: true,
      });
    }
    if (e.key === '2') {
      BacklogStore.setClickIssueDetail(item);
    }
  }

  handleClickDetail=() => {
    const { item } = this.props;
    BacklogStore.setClickIssueDetail(item);
  }

  /**
   *每个epic 右侧下拉选择项的menu
   *
   * @returns
   * @memberof FeatureItem
   */
  getmenu = () => {
    const { item, refresh } = this.props;
    return (
      <Menu onClick={this.clickMenu.bind(this)}>
        <div style={{ padding: '5px 12px' }}>
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
                  const inputData = {
                    colorCode: color.valueCode,
                    issueId: item.issueId,
                    objectVersionNumber: item.objectVersionNumber,
                  };
                  BacklogStore.axiosUpdateIssue(inputData).then((res) => {
                    BacklogStore.updateFeature(res);
                    refresh();
                  }).catch((error) => {
                  });
                }}
              />
            ))}
          </div>
        </div>
        <Menu.Divider />
        <Menu.Item key="1">编辑名称</Menu.Item>
        <Menu.Item key="2">查看史诗详情</Menu.Item>
      </Menu>
    );
  }

  /**
   *epic名称保存事件
   *
   * @param {*} e
   * @memberof FeatureItem
   */
  handleSave = (e) => {
    const { item, index, refresh } = this.props;
    e.stopPropagation();
    const { value } = e.target;
    if (item && item.epicName === value) {
      this.setState({
        editName: false,
      });
    } else {
      axios.get(`/agile/v1/projects/${AppState.currentMenuType.id}/issues/check_epic_name?epicName=${value}`)
        .then((checkRes) => {
          if (checkRes) {
            Choerodon.prompt('史诗名称重复');
          } else {
            this.setState({
              editName: false,
            });
            const req = {
              objectVersionNumber: item.objectVersionNumber,
              issueId: item.issueId,
              epicName: value,
            };
            BacklogStore.axiosUpdateIssue(req).then((res) => {
              BacklogStore.updateFeature(res);
              refresh();
            }).catch((error) => {
            });
          }
        });
    }
  }

  toggleExpand = (e) => {
    e.stopPropagation();
    const { expand } = this.state;
    this.setState({
      expand: !expand,
    });
  }

  render() {
    const {
      draggableProvided, item,
    } = this.props;
    const { expand, editName } = this.state;

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
            <p className="c7n-backlog-epicItemDetail">计数详情</p>
            <div role="none" onClick={this.handleClickDetail}>查看详情</div>
            <div className="c7n-backlog-epicItemParams">
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">问题数</p>
                <p className="c7n-backlog-epicItemNotStoryPoint">{item.storyCount}</p>
              </div>
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">已完成数</p>
                <p className="c7n-backlog-epicItemNotStoryPoint">{item.storyCompletedCount}</p>
              </div>
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">未预估数</p>
                <p className="c7n-backlog-epicItemNotStoryPoint">{item.unEstimateStoryCount}</p>
              </div>
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">故事点数</p>
                <p
                  className="c7n-backlog-epicItemParamValue"
                  style={{ minWidth: 31, color: 'rgba(0,0,0,0.65)' }}
                >
                  {item.totalStoryPoints}
                </p>
              </div>
            </div>
          </div>
        ) : ''}
      </div>
    );
  }
}

export default DraggableFeature;
