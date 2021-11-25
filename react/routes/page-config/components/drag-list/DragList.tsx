import React, { Component, Fragment, CSSProperties } from 'react';
import {
  DragDropContext, Droppable, Draggable, DraggingStyle, DropResult, NotDraggingStyle,
} from 'react-beautiful-dnd';
import { WindowScroller, List, AutoSizer } from 'react-virtualized';
import _ from 'lodash';
import { Choerodon } from '@choerodon/boot';
import {
  Card, Tooltip, Button, Input, Popconfirm,
} from 'choerodon-ui';
import { TextField } from 'choerodon-ui/pro';
import type { IntlShape } from 'react-intl';
import { Size } from 'choerodon-ui/lib/_util/enum';
import './DragList.less';
import { MAX_LENGTH_FIELD_OPTION_CODE, MAX_LENGTH_FIELD_OPTION_VALUE } from '@/constants/MAX_LENGTH';

interface OptionData {
  id: string,
  code: string,
  enabled: boolean,
  status: string,
  tempKey: string,
  value: string,
}
interface Props {
  disabled: boolean
  data: Array<OptionData>,
  onChange?: (data: Array<any>, status?: string) => void,
  onEdit?: (key: any, code: string, value: string) => void,
  onInvalid?: (key: any) => void,
  onActive?: (key: any) => void,
  onCreate?: (code: string, value: string) => void,
  onDelete?: (key: any) => void,
  formatMessage: IntlShape['formatMessage'],
  title?: string,
  tips: React.ReactNode,
}
interface StateProps {
  addItemVisible: boolean,
  tempKey: boolean | string,
  value: string,
  code: string,
  saveDisabled: boolean,
}

function getStyle({
  draggableStyle, virtualStyle, isDragging, item,
}: any) {
  let color = '#DDE7F2';
  if (isDragging) {
    color = '#DDE7F2';
  } else if (item.enabled) {
    color = '#FAFAFC';
  } else {
    color = '#F6F6F9';
  }
  const combined = {
    ...virtualStyle,
    ...draggableStyle,
  };

  const grid = 8;
  const height = isDragging ? combined.height : combined.height - grid;
  const result = {
    ...combined,
    left: isDragging ? combined.left : combined.left,
    width: isDragging
      ? draggableStyle.width
      : combined.width,
    marginBottom: grid,
    background: color,
    display: 'flex',
    alignItems: 'center',
    padding: '0 10px',
  };
  // eslint-disable-next-line no-restricted-globals
  if (!isNaN(height)) {
    result.height = height;
  }
  return result;
}
class DragList extends Component<Props, StateProps> {
  constructor(props: Props) {
    super(props);
    this.state = {
      addItemVisible: false,
      tempKey: false,
      value: '',
      code: '',
      saveDisabled: true,
    };
  }

  /**
   * 拖动完成时触发
   * @param result
   */
  // @ts-ignore
  onDragEnd = (result: DropResult) => {
    const { data, onChange } = this.props;
    const { source, destination } = result;

    // 拖拽到边框外
    if (!destination) {
      return;
    }

    // 排序
    const items = this.reorder(
      data,
      source.index,
      destination.index,
    );
    onChange && onChange(items);
  };

  // 开始拖动回调
  onDragStart = () => {
    this.setState({
      addItemVisible: false,
      tempKey: false,
    });
  };

  /**
   * 排序
   * @param list
   * @param startIndex
   * @param endIndex
   * @returns {Array}
   */
  reorder = (list: Array<OptionData>, startIndex: number, endIndex: number) => {
    const result = Array.from(list);
    const [removed] = result.splice(startIndex, 1);
    result.splice(endIndex, 0, removed);
    return result;
  };

  addItem = () => {
    this.setState({
      addItemVisible: true,
      tempKey: false,
    });
  };

  editItem = (tempKey: any) => {
    const { data } = this.props;
    const editItem = data.filter((item) => item.tempKey === tempKey || item.id === tempKey);
    this.setState({
      tempKey,
      addItemVisible: false,
      code: (editItem && editItem[0].code) || '',
      value: (editItem && editItem[0].value) || '',
    });
  };

  edit = (tempKey: any) => {
    const { data, onEdit, onChange } = this.props;
    const { code, value } = this.state;
    if (data.filter((item) => item.tempKey !== tempKey
      && item.id !== tempKey && item.value === value).length) {
      Choerodon.prompt('字段值不能重复！');
    } else if (data.filter((item) => item.tempKey !== tempKey
      && item.id !== tempKey && item.code === code).length) {
      Choerodon.prompt('字段编码不能重复！');
    } else {
      if (onEdit) {
        onEdit(tempKey, code, value);
      }
      if (onChange) {
        const updatedData = data.map((d) => {
          if (d.tempKey === tempKey || d.id === tempKey) {
            return {
              ...d,
              code,
              value,
              status: d.id ? 'update' : 'add',
            };
          }
          return d;
        });
        onChange(updatedData, 'edit');
      }
      this.cancel();
    }
  };

  invalid = (tempKey: any) => {
    const { data, onInvalid, onChange } = this.props;
    if (onInvalid) {
      onInvalid(tempKey);
    }
    if (onChange) {
      const updatedData = data.map((d) => {
        if (d.tempKey === tempKey || d.id === tempKey) {
          return { ...d, enabled: false };
        }
        return d;
      });
      onChange(updatedData, 'invalid');
    }
    this.cancel();
  };

  active = (tempKey: any) => {
    const { data, onActive, onChange } = this.props;
    if (onActive) {
      onActive(tempKey);
    }
    if (onChange) {
      const updatedData = data.map((d) => {
        if (d.tempKey === tempKey || d.id === tempKey) {
          return { ...d, enabled: true };
        }
        return d;
      });
      onChange(updatedData, 'active');
    }
    this.cancel();
  };

  create = () => {
    const { onCreate, data } = this.props;
    const { code, value } = this.state;
    if (_.find(data, { value })) {
      Choerodon.prompt('字段值不能重复！');
    } else if (_.find(data, { code })) {
      Choerodon.prompt('字段编码不能重复！');
    } else {
      if (onCreate) {
        onCreate(code, value);
      }
      this.cancel();
    }
  };

  remove = (tempKey: any) => {
    const { data, onDelete, onChange } = this.props;
    if (onDelete) {
      onDelete(tempKey);
    }
    if (onChange) {
      const updatedData = data.filter((d) => d.tempKey !== tempKey && d.id !== tempKey);
      onChange(updatedData, 'delete');
    }
    this.cancel();
  };

  cancel = () => {
    this.setState({
      addItemVisible: false,
      saveDisabled: true,
      tempKey: false,
      value: '',
      code: '',
    });
  };

  onValueChange = (value: any) => {
    const { code } = this.state;
    if (value) {
      this.setState({
        saveDisabled: !code,
        value,
      });
    } else {
      this.setState({
        saveDisabled: true,
        value: '',
      });
    }
  };

  onCodeChange = (v: string) => {
    const { value } = this.state;
    if (v) {
      this.setState({
        saveDisabled: !value,
        code: v,
      });
    } else {
      this.setState({
        saveDisabled: true,
        code: '',
      });
    }
  };

  renderItem = ({
    index, style, provided, isDragging, state,
  }: any) => {
    const { data, formatMessage, disabled } = this.props;

    const { tempKey, saveDisabled } = state;

    const item = data[index];
    return (
      <div
        ref={provided.innerRef}
        {...provided.draggableProps}
        {...provided.dragHandleProps}
        style={{
          ...getStyle({
            draggableStyle: provided.draggableProps.style,
            virtualStyle: style,
            isDragging,
            item,
          }),
        }}
      >
        {item.id === tempKey || item.tempKey === tempKey
          ? (
            <>
              <div className="issue-dragList-input">
                <TextField
                  defaultValue={item.code}
                  onChange={this.onCodeChange}
                  label={formatMessage({ id: 'dragList.placeholder.code' })}
                  maxLength={MAX_LENGTH_FIELD_OPTION_CODE}
                  valueChangeAction={'input' as any}
                />
              </div>
              <div className="issue-dragList-input">
                <TextField
                  defaultValue={item.value}
                  onChange={this.onValueChange}
                  label={formatMessage({ id: 'dragList.placeholder' })}
                  maxLength={MAX_LENGTH_FIELD_OPTION_VALUE}
                  valueChangeAction={'input' as any}
                />
              </div>
              <Button
                disabled={saveDisabled}
                type="primary"
                size={'small' as Size}
                onClick={() => this.edit(tempKey)}
                funcType="raised"
                style={{ marginLeft: 10 }}
                className="issue-dragList-add"
              >
                {formatMessage({ id: 'save' })}
              </Button>
              <Button
                size={'small' as Size}
                onClick={this.cancel}
                funcType="raised"
              >
                {formatMessage({ id: 'cancel' })}
              </Button>
            </>
          )
          : (
            <>
              <span className="issue-dragList-text">{item.code}</span>
              <span className="issue-dragList-text">{item.value}</span>
              {!disabled && (
                <div className="issue-dragList-operate">
                  <Tooltip
                    placement="bottom"
                    title={formatMessage({ id: 'edit' })}
                  >
                    <Button
                      size={'small' as Size}
                      shape="circle"
                      icon="edit-o"
                      onClick={() => this.editItem(item.tempKey || item.id)}
                    />
                  </Tooltip>
                  {
                    item.enabled
                      ? (
                        <Tooltip
                          placement="bottom"
                          title={formatMessage({ id: 'dragList.invalid' })}
                        >
                          <Button size={'small' as Size} icon="block" shape="circle" onClick={() => this.invalid(item.tempKey || item.id)} />
                        </Tooltip>
                      )
                      : (
                        <Tooltip
                          placement="bottom"
                          title={formatMessage({ id: 'dragList.active' })}
                        >
                          <Button size={'small' as Size} icon="playlist_add_check" shape="circle" onClick={() => this.active(item.tempKey || item.id)} />
                        </Tooltip>
                      )
                  }
                  <Tooltip
                    placement="bottom"
                    title={formatMessage({ id: 'delete' })}
                  >
                    <Popconfirm
                      placement="top"
                      title={`确认要删除 ${item.value} 吗？工作项上该字段值也会被清空。`}
                      onConfirm={() => this.remove(item.tempKey || item.id)}
                      okText="删除"
                      cancelText="取消"
                    >
                      <Button size={'small' as Size} shape="circle" icon="delete_sweep-o" />
                    </Popconfirm>
                  </Tooltip>
                </div>
              )}
            </>
          )}
      </div>
    );
  }

  rowRenderer = ({ index, style, state }: any) => {
    const { data, disabled } = this.props;

    const item = data[index];
    return (
      <Draggable
        isDragDisabled={disabled}
        key={item.tempKey || item.id}
        draggableId={String(item.tempKey || item.id)}
        index={index}
      >
        {(provided, snapshot) => (
          this.renderItem({
            index, style, provided, isDragging: snapshot.isDragging, state,
          })
        )}
      </Draggable>
    );
  }

  render() {
    const {
      data, tips, formatMessage, disabled,
    } = this.props;

    const { addItemVisible, tempKey, saveDisabled } = this.state;

    return (
      <div className="issue-dragList">
        <div className="issue-dragList-des">
          {tips}
        </div>
        <DragDropContext onDragEnd={this.onDragEnd} onDragStart={this.onDragStart}>
          <div className="issue-dragList-content">
            <Card
              title={(
                <>
                  <span style={{ display: 'inline-block', width: 'calc(50% - 52px)' }}>值</span>
                  <span>显示值</span>
                </>
              )}
              bordered={false}
              className="issue-dragList-card"
            >
              <Droppable
                isDropDisabled={disabled}
                droppableId="droppable"
                mode="virtual"
                renderClone={(provided, snapshot, rubric) => {
                  const { index } = rubric.source;
                  const issueObj = data[index];
                  if (!issueObj) {
                    return (
                      <div
                        ref={provided.innerRef}
                        {...provided.draggableProps}
                        {...provided.dragHandleProps}
                      />
                    );
                  }
                  return this.renderItem({
                    style: { margin: 0 },
                    index,
                    isDragging: snapshot.isDragging,
                    provided,
                    state: this.state,
                  });
                }}
              >
                {(provided) => (
                  <div
                    ref={provided.innerRef}
                    className="issue-issueTypeDrag-drop"
                  >
                    <WindowScroller scrollElement={document.getElementsByClassName('c7n-pro-modal-body')[0]}>
                      {({ height, scrollTop, registerChild }) => (
                        <div ref={(el) => registerChild(el)} style={{ width: '100%' }}>
                          <List
                            autoHeight={!disabled}
                            overscanRowCount={5}
                            height={disabled ? 400 : height}
                            rowCount={data.length}
                            rowHeight={disabled ? 40 : 80}
                            rowRenderer={({ index, style }) => this.rowRenderer({
                              index,
                              style,
                              state: this.state,
                            })}
                            scrollTop={disabled ? undefined : scrollTop}
                            width={656}
                          />
                        </div>
                      )}
                    </WindowScroller>
                    {addItemVisible
                      ? (
                        <div className="issue-dragList-addItem">
                          <span className="issue-dragList-input">
                            <TextField
                              onChange={this.onCodeChange}
                              label={formatMessage({ id: 'dragList.placeholder.code' })}
                              maxLength={MAX_LENGTH_FIELD_OPTION_CODE}
                              valueChangeAction={'input' as any}
                            />
                          </span>
                          <span className="issue-dragList-input">
                            <TextField
                              onChange={this.onValueChange}
                              label={formatMessage({ id: 'dragList.placeholder' })}
                              maxLength={MAX_LENGTH_FIELD_OPTION_VALUE}
                              valueChangeAction={'input' as any}
                            />
                          </span>
                          <div className="issue-dragList-btns">
                            <Button
                              type="primary"
                              size={'small' as Size}
                              onClick={this.create}
                              funcType="raised"
                              className="issue-dragList-add"
                              disabled={saveDisabled}
                            >
                              {formatMessage({ id: 'save' })}
                            </Button>
                            <Button
                              size={'small' as Size}
                              onClick={this.cancel}
                              funcType="raised"
                            >
                              {formatMessage({ id: 'cancel' })}
                            </Button>
                          </div>
                        </div>
                      )
                      : ''}
                    {provided.placeholder}
                  </div>
                )}
              </Droppable>
              {!disabled && (
                <Button
                  onClick={this.addItem}
                  funcType="flat"
                  className="issue-dragList-addBtn"
                >
                  <i className="icon-playlist_add icon" />
                  {formatMessage({ id: 'add' })}
                </Button>
              )}
            </Card>
          </div>
        </DragDropContext>
      </div>
    );
  }
}

export default DragList;
