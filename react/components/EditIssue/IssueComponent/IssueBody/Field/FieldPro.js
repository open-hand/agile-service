import { observer } from 'mobx-react';
import { toJS } from 'mobx';
import moment from 'moment';
import classnames from 'classnames';
import React, { Component, forwardRef } from 'react';
import { fieldApi } from '@/api';
import UserTag from '@/components/tag/user-tag';
import TextEditToggle from '@/components/TextEditTogglePro';
import { MAX_NUMBER_VALUE, MAX_NUMBER_STEP } from '@/constants/MAX_VALUE';
import { getEditFields } from '@/components/field-pro/layouts';

/**
 * 兼容性自定义编辑
 * @param {*} props
 */
const CompatibilityEditor = forwardRef(({ fieldType, ...otherProps }, ref) => {
  const editor = getEditFields([], [], { fieldType, outputs: ['element'] })[0][0];
  return React.cloneElement(editor, { ref, ...otherProps, showLengthInfo: fieldType === 'input' });
});
CompatibilityEditor.displayName = 'CompatibilityEditor';
@observer class FieldPro extends Component {
  updateIssueField = (value) => {
    const {
      store, onUpdate, reloadIssue, field, setIssueLoading,
    } = this.props;
    const issue = store.getIssue;
    const {
      fieldId, fieldType, fieldCode,
    } = field;
    let newValue = value;
    if (fieldType === 'time' || fieldType === 'datetime' || fieldType === 'date') {
      newValue = value && value.format('YYYY-MM-DD HH:mm:ss');
    }
    const { issueId } = issue;
    const obj = {
      fieldType,
      value: newValue,
    };
    setIssueLoading(true);
    store.setUpdateLoaded(false);
    fieldApi.project(store.projectId).updateFieldValue(issueId, fieldId, fieldCode, 'agile_issue', obj)
      .then(async () => {
        if (onUpdate) {
          onUpdate(issue);
        }
        if (reloadIssue) {
          await reloadIssue(issueId);
          store.setUpdateLoaded(true);
        }
      }).catch(() => {
        setIssueLoading(false);
      });
  };

  transform = (fieldType, value) => {
    if (fieldType === 'time' || fieldType === 'datetime' || fieldType === 'date') {
      return value ? moment(value) : undefined;
    } if (value instanceof Array) {
      return value.slice();
    }
    return value;
  };

  get value() {
    const {
      value: observableValue,
    } = this.props.field;
    const value = toJS(observableValue);
    return value;
  }

  renderEditor = () => {
    const { field, store } = this.props;
    const {
      fieldType, valueStr, extraConfig, fieldId,
    } = field;
    const { value } = this;
    const required = field?.required || store.getRuleRequired(field);
    const Editor = CompatibilityEditor;
    if (Editor) {
      switch (fieldType) {
        case 'single':
        case 'multiple':
        case 'radio':
        case 'checkbox':
        {
          return (
            <Editor
              fieldType={fieldType}
              searchable
              selected={value}
              required={required}
              fieldId={fieldId}
              projectId={store.projectId}
                // 始终为项目层查询接口
              menuType="project"
              multiple={fieldType === 'multiple' || fieldType === 'checkbox'}
              {
                ...store.getOptionsData(field, value)
                }
            />
          );
        }
        case 'text': {
          return <Editor fieldType={fieldType} required={required} autoSize />;
        }
        case 'multiMember': {
          return <Editor fieldType={fieldType} required={required} projectId={store.projectId} multiple selectedUser={valueStr} />;
        }
        case 'member': {
          return <Editor fieldType={fieldType} required={required} projectId={store.projectId} selectedUser={valueStr} />;
        }
        case 'number': {
          return <Editor fieldType={fieldType} required={required} max={MAX_NUMBER_VALUE} step={extraConfig ? MAX_NUMBER_STEP : 1} />;
        }
        default: return <Editor fieldType={fieldType} required={required} />;
      }
    }
    return null;
  }

  render() {
    const { field, disabled } = this.props;
    const {
      fieldName, fieldType, valueStr,
    } = field;
    const submitTrigger = ['blur'];
    const submitOnChange = ['member', 'single', 'radio'].includes(fieldType);
    if (submitOnChange) {
      submitTrigger.push('change');
    }
    const submitOnOut = ['radio', 'checkbox'].includes(fieldType);
    if (submitOnOut) {
      submitTrigger.push('click');
    }
    return (
      <div className={classnames('line-start mt-10', { 'line-start-100': fieldType === 'text' })}>
        <div className="c7n-property-wrapper">
          <span className="c7n-property">
            {`${fieldName}`}
          </span>
        </div>
        <div className="c7n-value-wrapper" style={{ width: 'auto' }}>
          <TextEditToggle
            disabled={disabled}
            alwaysRender={!['time', 'date', 'datetime'].includes(fieldType)}
            onSubmit={this.updateIssueField}
            initValue={this.transform(fieldType, this.value)}
            editor={this.renderEditor}
            submitTrigger={submitTrigger}
          >
            <div style={{ maxWidth: fieldType === 'text' ? '100%' : 200, wordBreak: 'break-all', whiteSpace: 'pre-line' }}>
              {['member', 'multiMember'].includes(fieldType) && valueStr
                ? <UserTag data={valueStr} /> : (valueStr || '无')}
            </div>
          </TextEditToggle>
        </div>
      </div>
    );
  }
}

export default FieldPro;
