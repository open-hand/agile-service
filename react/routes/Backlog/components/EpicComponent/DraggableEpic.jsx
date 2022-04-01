import React, { Component } from 'react';
import { Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import {
  Dropdown, Menu, Input, Icon, Tooltip,
} from 'choerodon-ui';
import _ from 'lodash';
import { epicApi, issueApi } from '@/api';
import BacklogStore from '../../../../stores/project/backlog/BacklogStore';

// @inject('AppState')
@observer
class DraggableEpic extends Component {
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
   * @memberof EpicItem
   */
  clickMenu = (e) => {
    const { item } = this.props;

    if (e && e.stopPropagation) {
      e.stopPropagation();
    } else if (e && e.domEvent && e.domEvent.stopPropagation) {
      e.domEvent.stopPropagation();
    }
    this.toggleClickColor(false);
    if (e.key === '1') {
      this.setState({
        editName: true,
      });
    }
    if (e.key === '2') {
      BacklogStore.setClickIssueDetail(item);
    }
  }

  /**
   *每个epic 右侧下拉选择项的menu
   *
   * @returns
   * @memberof EpicItem
   */
  getmenu = () => {
    const { item, refresh } = this.props;
    return (
      // eslint-disable-next-line react/jsx-no-bind
      <Menu onClick={this.clickMenu.bind(this)}>
        <div style={{ padding: '5px 12px', marginTop: '10px' }}>
          颜色
          <div className="c7n-backlog-epicColor">
            {BacklogStore.getColorLookupValue.map((color) => (
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
                  this.toggleClickColor(false);
                  BacklogStore.updateEpic({ ...item || {}, color: color.name });
                  issueApi.update(inputData).then((res) => {
                    BacklogStore.updateEpic(res);
                    refresh();
                  }).catch(() => {
                    BacklogStore.updateEpic(item);
                  });
                }}
              />
            ))}
          </div>
        </div>
        <Menu.Divider />
        <Menu.Item key="1">修改名称</Menu.Item>
        <Menu.Item key="2">查看史诗详情</Menu.Item>
      </Menu>
    );
  }

  /**
   *epic名称保存事件
   *
   * @param {*} e
   * @memberof EpicItem
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
      epicApi.checkName(value, item.issueId)
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
            issueApi.update(req).then((res) => {
              BacklogStore.updateEpic(res);
              refresh();
            }).catch(() => {
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
          onClickEpic: BacklogStore.getChosenEpic === item.issueId,
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
            <div className="c7n-backlog-epicItemsHead hidden-length-info">
              {editName ? (
                <Input
                  className="input-small"
                  autoFocus
                  defaultValue={item.epicName}
                  onPressEnter={this.handleSave}
                  onClick={(e) => {
                    e.stopPropagation();
                  }}
                  border={false}
                  onBlur={this.handleSave}
                  maxLength={20}
                />
              ) : (
                <Tooltip title={`史诗名称：${item.epicName}`}>
                  <p>{item.epicName}</p>
                </Tooltip>
              )}
              <Dropdown
                onClick={(e) => e.stopPropagation()}
                overlay={this.getmenu()}
                trigger={['click']}
                visible={colorVisible}
                onVisibleChange={this.toggleClickColor}
              >
                <Icon
                  style={{
                    width: 12,
                    height: 12,
                    background: item.color || '#f953ba',
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
                  flex: item.doneIssueCount,
                }}
              />
              <div
                className="c7n-backlog-epicItemTodo"
                style={{
                  flex: item.issueCount ? item.issueCount - item.doneIssueCount : 1,
                }}
              />
            </div>
          </div>
        </div>
        {expand ? (
          <div style={{ paddingLeft: 12 }}>
            <p className="c7n-backlog-epicItemDes">
              {_.isNull(item.summary) ? '没有描述' : item.summary}
            </p>
            <p className="c7n-backlog-epicItemDetail">计数详情</p>
            <div className="c7n-backlog-epicItemParams">
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">工作项数</p>
                <p className="c7n-backlog-epicItemNotStoryPoint">{item.issueCount}</p>
              </div>
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">已完成数</p>
                <p className="c7n-backlog-epicItemNotStoryPoint">{item.doneIssueCount}</p>
              </div>
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">未预估数</p>
                <p className="c7n-backlog-epicItemNotStoryPoint">{item.notEstimate}</p>
              </div>
              <div className="c7n-backlog-epicItemParam">
                <p className="c7n-backlog-epicItemParamKey">故事点数</p>
                <p
                  className="c7n-backlog-epicItemParamValue"
                  style={{ minWidth: 31, color: 'var(--text-color3)' }}
                >
                  {item.totalEstimate}
                </p>
              </div>
            </div>
          </div>
        ) : ''}
      </div>
    );
  }
}

export default DraggableEpic;
