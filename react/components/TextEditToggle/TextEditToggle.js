/* eslint-disable react/require-default-props */
/* eslint-disable react/static-property-placement */
/* eslint-disable react/no-find-dom-node, react/destructuring-assignment */
import React, { Component, createRef } from 'react';
import {
  Form, Icon, Select, DatePicker, Button,
} from 'choerodon-ui';
import { Choerodon } from '@choerodon/boot';
import classNames from 'classnames';
import { findDOMNode } from 'react-dom';
import PropTypes from 'prop-types';
import DefaultOpenSelect from '../DefaultOpenSelect';
import SelectFocusLoad from '../SelectFocusLoad';
import './TextEditToggle.less';

const { MonthPicker, RangePicker, WeekPicker } = DatePicker;
// 防止提交前变回原值
const Text = ({ children, newData, originData }) => (typeof (children) === 'function' ? children(newData || originData) : children);

const Edit = ({ children }) => children;
const FormItem = Form.Item;
function contains(root, n) {
  let node = n;
  while (node) {
    if (node === root) {
      return true;
    }
    node = node.parentNode;
  }

  return false;
}

class TextEditToggle extends Component {
  static defaultProps = {
    noButton: true,
  };

  static propTypes = {
    saveRef: PropTypes.func,
    className: PropTypes.string,
    disabled: PropTypes.bool,
    required: PropTypes.bool,
    noButton: PropTypes.bool,
    editButtonMode: PropTypes.bool,
    simpleMode: PropTypes.bool,
    formKey: PropTypes.string,
    onSubmit: PropTypes.func,
    onCancel: PropTypes.func,
    // eslint-disable-next-line react/forbid-prop-types
    originData: PropTypes.any,
    children: PropTypes.node,
  };

  constructor() {
    super();
    this.Edit = createRef();
    this.state = {
      editing: false,
      originData: null,
      newData: null,
    };
  }

  componentDidMount() {
    // eslint-disable-next-line no-unused-expressions
    this.exportRef();
  }

  exportRef() {
    const { saveRef } = this.props;
    if (saveRef) {
      if (typeof saveRef === 'function') {
        saveRef(this);
      }
      if (typeof saveRef === 'object') {
        saveRef.current = this;
      }
    }
  }

  static getDerivedStateFromProps(props, state) {
    if (props.originData !== state.originData) {
      return {
        originData: props.originData,
        newData: null,
      };
    }
    return null;
  }

  componentWillUnmount() {
    document.removeEventListener('mousedown', this.handleDocumentClick);
  }

  handleDocumentClick = (event) => {
    const { target } = event;
    const root = findDOMNode(this);
    // 如果点击不在当前元素内，就调用submit提交数据
    if (!this.PortalMouseDown && !contains(root, target)) {
      this.handleSubmit();
    }
    setTimeout(() => {
      this.PortalMouseDown = false;
    });
  }

  handlePortalMouseDown = () => {
    this.PortalMouseDown = true;
  }

  handleDone = () => {
    this.setState({
      newData: null,
    });
    const {
      required, fieldType, onSubmit, originData,
      formKey, form, fieldName,
    } = this.props;
    form.setFieldsValue({ [formKey]: originData });
  }

  // 提交编辑
  handleSubmit = () => {
    const {
      required, fieldType, onSubmit, originData,
      formKey, form, fieldName,
    } = this.props;
    try {
      form.validateFields((err, values) => {
        if (!err) {
          document.removeEventListener('mousedown', this.handleDocumentClick);
          if (formKey) {
            const newData = values[formKey];
            if (onSubmit && newData !== originData) {
              if (required) {
                if ((fieldType === 'number' && newData === undefined) || (fieldType !== 'number' && !newData)) {
                  form.setFieldsValue({ [formKey]: originData });
                  Choerodon.prompt(`${fieldName}必填！`);
                } else if (newData instanceof Array && newData.length === 0) {
                  form.setFieldsValue({ [formKey]: originData });
                  Choerodon.prompt(`${fieldName}必填！`);
                } else {
                  this.setState({
                    newData,
                  });
                  // 传入一个done方法，用于防止父组件数据更新后的newData错误工作项
                  onSubmit(formKey ? newData : null, this.handleDone);
                }
              } else {
                this.setState({
                  newData,
                });
                // 传入一个done方法，用于防止父组件数据更新后的newData错误工作项
                onSubmit(formKey ? newData : null, this.handleDone);
              }
            }
          } else {
            onSubmit();
          }
          this.setState({
            editing: false,
          });
        }
      });
    } catch (err) {
      this.setState({
        editing: false,
      });
    }
  }

  // 进入编辑状态
  enterEditing = () => {
    // 如果禁用，将不进入编辑模式
    const { disabled } = this.props;
    if (disabled) {
      return;
    }
    document.addEventListener('mousedown', this.handleDocumentClick);
    this.setState({
      editing: true,
      originData: this.props.originData,
      newData: null,
    });
  }

  // 取消编辑
  leaveEditing = () => {
    document.removeEventListener('mousedown', this.handleDocumentClick);
    this.setState({
      editing: false,
    });
    if (this.props.onCancel) {
      this.props.onCancel(this.state.originData);
    }
  }

  getEditOrTextChildren = () => {
    const { editing } = this.state;
    const { children } = this.props;
    return editing
      ? children.filter((child) => child.type === Edit)
      : children.filter((child) => child.type === Text);
  }

  getEditChildrenType = () => {
    const { children } = this.props;

    const EditChildren = children.filter((child) => child.type === Edit);
    const childrenArray = React.Children.toArray(EditChildren);
    const targetElement = React.Children.toArray(childrenArray[0].props.children)[0];
    // 替换成自动打开的Select
    if (targetElement && (targetElement.type === Select || targetElement.type === SelectFocusLoad)) {
      return 'Select';
    }
    return 'Input';
  }

  renderFormItemChild(children) {
    // formItem只有一个组件起作用
    const childrenArray = React.Children.toArray(children);
    const targetElement = childrenArray[0];
    if (!targetElement) {
      throw new Error('使用Form功能时，Edit的children必须是Component');
    }
    // 替换成自动打开的Select
    if (targetElement.type === Select) {
      if (targetElement.props.mode) {
        return <DefaultOpenSelect {...targetElement.props} />;
      }
      // 单选选择后自动提交
      return <DefaultOpenSelect {...targetElement.props} onSelect={() => setTimeout(this.handleSubmit)} />;
    } if ((targetElement.type === Select || targetElement.type === SelectFocusLoad) && !targetElement.props.mode) {
      return React.cloneElement(targetElement, {
        onSelect: () => setTimeout(this.handleSubmit),
      });
    } if (
      targetElement.type === DatePicker
      || targetElement.type === MonthPicker
      || targetElement.type === RangePicker
      || targetElement.type === WeekPicker) {
      return React.cloneElement(targetElement, {
        defaultOpen: true,
        onOpenChange: (open) => {
          if (!open) {
            setTimeout(this.handleSubmit);
          }
        },
      });
    }
    return targetElement;
  }

  // 为子元素加上getPopupContainer，因为默认getPopupContainer是body,点击时判断handleDocumentClick会调用onSubmit方法
  wrapChildren = (children) => {
    const childrenElement = typeof children === 'function'
      ? children({
        width: this.Edit.current ? this.Edit.current.clientWidth : undefined,
      })
      : children;
    const childrenArray = React.Children.toArray(childrenElement);
    return childrenArray.map((child) => {
      if (!child.props.getPopupContainer) {
        return React.cloneElement(child, {
          getPopupContainer: () => findDOMNode(this),
        });
      }
      return child;
    });
  }

  renderTextChild = (children) => {
    const childrenArray = React.Children.toArray(children);
    return childrenArray.map((child) => React.cloneElement(child, {
      newData: this.state.newData,
      originData: this.props.originData,
    }));
  }

  renderChild = () => {
    const { editing, newData } = this.state;
    const { disabled, simpleMode, noButton } = this.props;
    const {
      originData, formKey, rules, fieldProps, editButtonMode,
    } = this.props;
    const { getFieldDecorator } = this.props.form;
    // 拿到不同模式下对应的子元素
    const children = this.getEditOrTextChildren();
    const hoverType = this.getEditChildrenType();
    // 根据不同模式对子元素进行包装
    return editing ? (
      <div
        role="none"
        className="c7nagile-TextEditToggle-edit"
        onMouseDown={this.handlePortalMouseDown} // Portal的事件会冒泡回父组件
      >
        { // 采用form模式就进行form包装,否则
          formKey ? (
            <Form layout="vertical">
              {children.map((child) => (
                <FormItem>
                  {getFieldDecorator(formKey, {
                    rules,
                    initialValue: originData,
                    ...fieldProps,
                  })(
                    this.renderFormItemChild(this.wrapChildren(child.props.children)),
                  )}
                </FormItem>
              ))}
            </Form>
          ) : children.map((child) => (this.wrapChildren(child.props.children)))
        }
        {!noButton && !simpleMode && (
          <div>
            <div style={{ textAlign: 'right', lineHeight: '20px' }}>
              <Icon type="done" className="c7nagile-TextEditToggle-edit-icon" onClick={this.handleSubmit} />
              <Icon type="close" className="c7nagile-TextEditToggle-edit-icon" onClick={this.leaveEditing} />
            </div>
          </div>
        )}
      </div>
    ) : (
      <div
        className={classNames({
          'c7nagile-TextEditToggle-text': true,
          'c7nagile-TextEditToggle-text-active': !simpleMode && !disabled,
          [hoverType]: true,
          noButton,
        })}
        ref={this.Edit}
        onMouseDown={() => { this.timer = Date.now(); }}
        onMouseUp={(e) => {
          if (!editButtonMode && Date.now() - this.timer <= 200) {
            this.enterEditing(e);
          }
        }}
        role="none"
      >
        {this.renderTextChild(children)}
        {!simpleMode && <Icon type="arrow_drop_down" className="c7nagile-TextEditToggle-text-icon" />}
        {editButtonMode && (
        <Button
          className="c7nagile-TextEditToggle-text-edit-button"
          icon="edit-o"
          shape="circle"
          funcType="flat"
          onClick={() => { this.enterEditing(); }}
        />
        )}
      </div>
    );
  }

  ToggleBlur = (e) => {
    if (!this.PortalMouseDown) {
      this.handleSubmit();
    }
  }

  handleKeyDown = (e) => {
    const { submitOnEnter } = this.props;
    if (e.keyCode === 13 && submitOnEnter) {
      this.handleSubmit();
    }
  }

  render() {
    const { style, className, focusAble } = this.props;
    let extraProps = {};
    if (focusAble) {
      extraProps = {
        tabIndex: 0,
        onFocus: this.enterEditing,
        onBlur: this.ToggleBlur,
        onKeyDown: this.handleKeyDown,
      };
    }
    return (
      <div style={style} className={`c7nagile-TextEditToggle ${className || ''}`} {...extraProps}>
        {this.renderChild()}
      </div>
    );
  }
}
TextEditToggle.Text = Text;
TextEditToggle.Edit = Edit;

Text.propTypes = {
  children: PropTypes.oneOfType([
    PropTypes.element,
    PropTypes.func,
  ]).isRequired,
};
Edit.propTypes = {
  children: PropTypes.oneOfType([
    PropTypes.element,
    PropTypes.func,
  ]).isRequired,
};
export default Form.create({})(TextEditToggle);
