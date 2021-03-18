import React, { Component, Fragment, CSSProperties } from 'react';
import {
  DragDropContext, Droppable, Draggable, DraggingStyle, DropResult, NotDraggingStyle,
} from 'react-beautiful-dnd';
import _ from 'lodash';
import { Choerodon } from '@choerodon/boot';
import {
  Card, Tooltip, Button, Input, Popconfirm,
} from 'choerodon-ui';
import { InjectedIntl } from 'react-intl';
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
  data: Array<OptionData>,
  onChange?: (data: Array<any>, status?: string) => void,
  onEdit?: (key: any, code: string, value: string) => void,
  onInvalid?: (key: any) => void,
  onActive?: (key: any) => void,
  onCreate?: (code: string, value: string) => void,
  onDelete?: (key: any) => void,
  formatMessage: InjectedIntl['formatMessage'],
  title?: string,
  tips: string,
}
interface StateProps {
  addItemVisible: boolean,
  tempKey: boolean | string,
  value: string,
  code: string,
  saveDisabled: boolean,
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

  // 获取元素样式，根据是否拖动变化
  // @ts-ignore
  getItemStyle = (isDragging: boolean, draggableStyle: DraggingStyle | NotDraggingStyle | undefined,
    item: OptionData): CSSProperties => {
    let color = '#DDE7F2';
    if (isDragging) {
      color = '#DDE7F2';
    } else if (item.enabled) {
      color = '#F7F7F7';
    } else {
      color = '#F0F0F0';
    }
    return {
      userSelect: 'none',
      padding: '5px 20px',
      margin: '0 0 5px 0',
      background: color,
      height: 34,
      ...draggableStyle,
    };
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
    }, () => {
      const input = document.getElementById('dragList-input');
      if (input) {
        input.focus();
      }
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
    }, () => {
      const input = document.getElementById('dragList-input');
      if (input) {
        input.focus();
      }
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

  onValueChange = (e: any) => {
    const { code } = this.state;
    if (e.target.value) {
      this.setState({
        saveDisabled: !code,
        value: e.target.value,
      });
    } else {
      this.setState({
        saveDisabled: true,
        value: '',
      });
    }
  };

  onCodeChange = (e: React.ChangeEvent<any>) => {
    const { value } = this.state;
    if (e.target.value) {
      this.setState({
        saveDisabled: !value,
        code: e.target.value as string,
      });
    } else {
      this.setState({
        saveDisabled: true,
        code: '',
      });
    }
  };

  render() {
    const {
      data, tips, formatMessage,
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
                <span>
                  <span style={{ display: 'inline-block', width: '34%' }}>值</span>
                  <span>显示值</span>
                </span>
              )}
              bordered={false}
              className="issue-dragList-card"
            >
              <Droppable droppableId="droppable">
                {(provided: { innerRef: React.LegacyRef<HTMLDivElement> | undefined; placeholder: string | number | boolean | {} | React.ReactElement<any, string | React.JSXElementConstructor<any>> | React.ReactNodeArray | React.ReactPortal | null | undefined; }) => (
                  <div
                    ref={provided.innerRef}
                    className="issue-issueTypeDrag-drop"
                  >
                    {data && data.map((item, index) => (
                      <Draggable
                        key={item.tempKey || item.id}
                        draggableId={String(item.tempKey || item.id)}
                        index={index}
                      >
                        {(subProvided: { innerRef: React.LegacyRef<HTMLDivElement> | undefined; draggableProps: JSX.IntrinsicAttributes & React.ClassAttributes<HTMLDivElement> & React.HTMLAttributes<HTMLDivElement>; dragHandleProps: JSX.IntrinsicAttributes & React.ClassAttributes<HTMLDivElement> & React.HTMLAttributes<HTMLDivElement>; }, subSnapshot: { isDragging: boolean; }) => (
                          <div
                            ref={subProvided.innerRef}
                            {...subProvided.draggableProps}
                            {...subProvided.dragHandleProps}
                            style={this.getItemStyle(
                              subSnapshot.isDragging,
                              subProvided.draggableProps.style,
                              item,
                            )}
                          >
                            {item.id === tempKey || item.tempKey === tempKey
                              ? (
                                <>
                                  <span className="issue-dragList-input">
                                    <Input
                                      id="dragList-code"
                                      defaultValue={item.code}
                                      onChange={this.onCodeChange}
                                      className="hidden-label"
                                      placeholder={formatMessage({ id: 'dragList.placeholder.code' })}
                                      maxLength={10}
                                    />
                                  </span>
                                  <span className="issue-dragList-input">
                                    <Input
                                      id="dragList-value"
                                      defaultValue={item.value}
                                      onChange={this.onValueChange}
                                      className="hidden-label"
                                      placeholder={formatMessage({ id: 'dragList.placeholder' })}
                                      maxLength={10}
                                    />
                                  </span>
                                  <Button
                                    disabled={saveDisabled}
                                    type="primary"
                                    size={'small' as Size}
                                    onClick={() => this.edit(tempKey)}
                                    funcType="raised"
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
                                  <div className="issue-dragList-operate">
                                    <Tooltip
                                      placement="bottom"
                                      title={formatMessage({ id: 'edit' })}
                                    >
                                      <Button
                                        size={'small' as Size}
                                        shape="circle"
                                        onClick={() => this.editItem(item.tempKey || item.id)}
                                      >
                                        <i className="icon icon-mode_edit" />
                                      </Button>
                                    </Tooltip>
                                    {
                                      item.enabled
                                        ? (
                                          <Tooltip
                                            placement="bottom"
                                            title={formatMessage({ id: 'dragList.invalid' })}
                                          >
                                            <Button size={'small' as Size} shape="circle" onClick={() => this.invalid(item.tempKey || item.id)}>
                                              <i className="icon icon-block" />
                                            </Button>
                                          </Tooltip>
                                        )
                                        : (
                                          <Tooltip
                                            placement="bottom"
                                            title={formatMessage({ id: 'dragList.active' })}
                                          >
                                            <Button size={'small' as Size} shape="circle" onClick={() => this.active(item.tempKey || item.id)}>
                                              <i className="icon icon-playlist_add_check" />
                                            </Button>
                                          </Tooltip>
                                        )
                                    }
                                    <Tooltip
                                      placement="bottom"
                                      title={formatMessage({ id: 'delete' })}
                                    >
                                      <Popconfirm
                                        placement="top"
                                        title={`确认要删除 ${item.value} 吗？问题上该字段值也会被清空。`}
                                        onConfirm={() => this.remove(item.tempKey || item.id)}
                                        okText="删除"
                                        cancelText="取消"
                                      >
                                        <Button size={'small' as Size} shape="circle">
                                          <i className="icon icon-delete" />
                                        </Button>
                                      </Popconfirm>
                                    </Tooltip>
                                  </div>
                                </>
                              )}
                          </div>
                        )}
                      </Draggable>
                    ))}
                    {addItemVisible
                      ? (
                        <div className="issue-dragList-addItem">
                          <span className="issue-dragList-input">
                            <Input
                              id="dragList-code"
                              onChange={this.onCodeChange}
                              className="hidden-label"
                              placeholder={formatMessage({ id: 'dragList.placeholder.code' })}
                              maxLength={MAX_LENGTH_FIELD_OPTION_CODE}
                            />
                          </span>
                          <span className="issue-dragList-input">
                            <Input
                              id="dragList-value"
                              onChange={this.onValueChange}
                              className="hidden-label"
                              placeholder={formatMessage({ id: 'dragList.placeholder' })}
                              maxLength={MAX_LENGTH_FIELD_OPTION_VALUE}
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
              <Button
                onClick={this.addItem}
                funcType="flat"
                className="issue-dragList-addBtn"
              >
                <i className="icon-playlist_add icon" />
                {formatMessage({ id: 'add' })}
              </Button>
            </Card>
          </div>
        </DragDropContext>
      </div>
    );
  }
}

export default DragList;
