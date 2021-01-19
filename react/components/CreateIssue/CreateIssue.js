/* eslint-disable react/jsx-closing-tag-location */
import React, { Component } from 'react';
import {
  stores, Content, Choerodon, Permission,
} from '@choerodon/boot';
import { map, find } from 'lodash';
import {
  Select, Form, Input, Button, Modal, Spin, Tooltip,
} from 'choerodon-ui';
import moment from 'moment';
import reactComponentDebounce from '@choerodon/react-component-debounce';
import {
  featureApi, epicApi, fieldApi, issueTypeApi,
  issueApi,
  pageConfigApi,
} from '@/api';
import {
  uploadAndReplaceImg, handleFileUpload, validateFile, normFile, text2Delta,
} from '@/utils/richText';
import {
  getProjectName, getProjectId,
} from '@/utils/common';
import { observer } from 'mobx-react';
import { IsProjectMember } from '@/hooks/useIsProjectMember';
import { IsInProgram } from '@/hooks/useIsInProgram';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { UploadButton } from '../CommonComponent';
import SelectNumber from '../SelectNumber';
import WYSIWYGEditor from '../WYSIWYGEditor';
import TypeTag from '../TypeTag';
import './CreateIssue.less';
import SelectFocusLoad from '../SelectFocusLoad';
import renderField from './renderField';
import FieldIssueLinks from './FieldIssueLinks';
import WSJF from './WSJF';
import FieldTeam from './FieldTeam';
import FieldStartTime from './FieldStartTime';
import FieldEndTime from './FieldEndTime';
import { OldSelectProgramVersion as SelectProgramVersion } from '../select/select-program-version';
import { OldSelectEnvironment as SelectEnvironment } from '../select/select-environment';

const DebounceInput = reactComponentDebounce({
  valuePropName: 'value',
  triggerMs: 250,
})(Input);
const DebounceEditor = reactComponentDebounce({
  valuePropName: 'value',
  triggerMs: 250,
})(WYSIWYGEditor);

const { AppState } = stores;
const { Sidebar } = Modal;
const { Option } = Select;
const FormItem = Form.Item;
const defaultProps = {
  mode: 'default',
  applyType: 'agile',
  request: issueApi.create,
  defaultTypeCode: 'story',
  title: '创建问题',
  contentTitle: `在项目“${getProjectName()}”中创建问题`,
  contentDescription: '请在下面输入问题的详细信息，包含详细描述、人员信息、版本信息、进度预估、优先级等等。您可以通过丰富的任务描述帮助相关人员更快更全面的理解任务，同时更好的把控问题进度。',
  contentLink: 'http://v0-16.choerodon.io/zh/docs/user-guide/agile/agile/create-agile/',
  hiddenFields: ['pi'],
};
function applyFilter(array, filters) {
  const Filters = [];
  filters.forEach((filter) => {
    if (typeof filter === 'function') {
      Filters.push(filter);
    } else if (typeof filter === 'object') {
      if (filter.apply && typeof filter.filter === 'function') {
        Filters.push(filter.filter);
      }
    }
  });
  let result = array;
  Filters.forEach((filter) => {
    result = result.filter(filter);
  });
  return result;
}
class CreateIssue extends Component {
  constructor(props) {
    super(props);
    this.state = {
      createLoading: false,
      loading: true,
      originLabels: [],
      originComponents: [],
      originIssueTypes: [],
      defaultTypeId: false,
      newIssueTypeCode: '',
      fields: [],
    };
    this.originDescription = true;
  }

  componentDidMount() {
    this.loadIssueTypes();
  }

  /**
   * 为子任务和子bug设置默认冲刺
   *
   * @memberof CreateIssue
   */
  setDefaultSprint = () => {
    const { mode, parentIssueId, relateIssueId } = this.props;
    if (['sub_task', 'sub_bug'].includes(mode)) {
      issueApi.load(parentIssueId || relateIssueId).then((res) => {
        const { form: { setFieldsValue } } = this.props;
        const { activeSprint } = res;
        if (activeSprint) {
          setFieldsValue({
            sprintId: activeSprint.sprintId,
          });
        }
      });
    }
  }

  autoSetSprint = (issueId) => {
    const { form: { setFieldsValue } } = this.props;
    if (issueId) {
      issueApi.load(issueId).then((res) => {
        const { activeSprint } = res;
        if (activeSprint) {
          setFieldsValue({
            sprintId: activeSprint.sprintId,
          });
        } else {
          setFieldsValue({
            sprintId: undefined,
          });
        }
      });
    } else {
      setFieldsValue({
        sprintId: undefined,
      });
    }
  }

  // eslint-disable-next-line react/destructuring-assignment
  getDefaultType = (issueTypes = this.state.originIssueTypes) => {
    const { defaultTypeCode } = this.props;
    return find(issueTypes, { typeCode: defaultTypeCode });
  }

  handleSave = (data, fileList) => {
    const { fields, newIssueTypeCode } = this.state;
    const {
      onOk, form, request, applyType,
    } = this.props;
    request(data, applyType).then((res) => {
      const fieldList = [];
      fields.forEach((item) => {
        if (!item.system) {
          let value = form.getFieldValue(item.fieldCode);
          if (item.fieldType === 'time' || item.fieldType === 'datetime' || item.fieldType === 'date') {
            value = value && value.format('YYYY-MM-DD HH:mm:ss');
          }
          fieldList.push({
            fieldType: item.fieldType,
            value,
            fieldId: item.fieldId,
            fieldCode: item.fieldCode,
          });
        }
      });
      fieldApi.createFieldValue(res.issueId, 'agile_issue', fieldList);
      if (fileList && fileList.length > 0) {
        const config = {
          issueType: res.statusId,
          issueId: res.issueId,
          fileName: fileList[0].name,
          projectId: AppState.currentMenuType.id,
        };
        if (fileList.some((one) => !one.url)) {
          handleFileUpload(fileList, () => { }, config);
        }
      }
      if (newIssueTypeCode === 'feature' && data.programVersion) {
        featureApi.updateVersions(res.issueId, data.programVersion);
      }
      form.resetFields();
      this.setState({
        createLoading: false,
      });
      onOk(res, data);
      Choerodon.prompt(`${res.issueNum || ''}创建成功`, 'success');
    }).catch((e) => {
      form.resetFields();
      this.setState({
        createLoading: false,
      });
      onOk();
    });
  };

  checkSameDescription = (origin, current) => {
    if (!origin) {
      return !current || JSON.stringify(current) === JSON.stringify([{ insert: '\n' }]);
    }
    if (current) {
      return origin === JSON.stringify(current);
    }
    return true;
  };

  loadDefaultTemplate = (issueTypeId) => {
    const { form } = this.props;
    if (this.props.defaultDescription) { // 如果有外部默认描述信息则 放弃加载默认模版
      return;
    }
    const currentDes = form.getFieldValue('description');
    pageConfigApi.loadTemplateByType(issueTypeId).then((res) => {
      const { template } = res || {};
      form.setFieldsValue({
        description: text2Delta(template),
      });
      if (!template) {
        form.setFieldsValue({
          description: currentDes,
        });
      }
    });
  }

  setDefaultValue = (fields) => {
    const { form } = this.props;
    const defaultScope = new Map([
      ['assignee', 'assigneedId'],
      ['component', 'componentIssueRel'],
      ['summary', 'summary'],
      ['label', 'issueLabel'],
      ['fixVersion', 'fixVersionIssueRel'],
      ['sprint', 'sprintId'],
      ['epic', 'epicId'],
      // ['pi', 'pi'],
    ]);
    const setFields = fields.reduce((result, field) => {
      const name = defaultScope.get(field.fieldCode);
      if (name && field.defaultValue) {
        if (!form.getFieldValue(name)) {
          if (name === 'componentIssueRel') {
            Object.assign(result, {
              [name]: field.defaultValueObjs?.map((c) => c.name),
            });
          } else if (name === 'issueLabel') {
            Object.assign(result, {
              [name]: field.defaultValueObjs?.map((c) => c.labelName),
            });
          } else {
            Object.assign(result, {
              [name]: field.defaultValue,
            });
          }
        }
      }
      return result;
    }, {});
    form.setFieldsValue(setFields);
  }

  loadIssueTypes = () => {
    const { applyType, form } = this.props;

    issueTypeApi.loadAllWithStateMachineId(applyType).then((res) => {
      if (res && res.length) {
        const defaultType = this.getDefaultType(res);
        const param = {
          schemeCode: 'agile_issue',
          context: defaultType.typeCode,
          pageCode: 'agile_issue_create',
        };
        this.loadDefaultTemplate(defaultType.typeCode);
        fieldApi.getFields(param).then((fields) => {
          this.setState({
            fields,
            originIssueTypes: res,
            defaultTypeId: defaultType.id,
            loading: false,
            newIssueTypeCode: defaultType.typeCode,
          }, () => {
            this.setDefaultSprint();
            this.setDefaultValue(fields);
          });
        });
      }
    });
  };

  getIssueLinks = (keys, linkTypes, linkIssues) => {
    const issueLinkCreateVOList = [];
    if (keys && linkTypes && linkIssues) {
      keys.forEach((key) => {
        const link = linkTypes[`${key}]`];
        const issues = linkIssues[`${key}]`];
        const [linkTypeId, isIn] = link.split('+');

        if (issues) {
          issues.forEach((issueId) => {
            issueLinkCreateVOList.push({
              linkTypeId,
              linkedIssueId: issueId,
              in: isIn === 'true',
            });
          });
        }
      });
    }
    return issueLinkCreateVOList;
  }

  handleCreateIssue = () => {
    const { form, parentIssueId, relateIssueId } = this.props;
    const {
      fields,
      originComponents,
      originLabels,
      originIssueTypes,
    } = this.state;
    form.validateFieldsAndScroll(async (err, values) => {
      if (!err) {
        const {
          typeId,
          summary,
          description,
          storyPoints,
          estimatedTime,
          sprintId,
          epicId,
          pi,
          epicName,
          assigneedId,
          benfitHypothesis,
          acceptanceCritera,
          featureType,
          componentIssueRel,
          priorityId,
          issueLabel,
          fixVersionIssueRel,
          linkTypes,
          linkIssues,
          keys,
          fileList,
          userBusinessValue,
          timeCriticality,
          rrOeValue,
          jobSize,
          featureId,
          teamProjectIds,
          estimatedEndTime,
          estimatedStartTime,
          subBugParent,
          subTaskParent,
          programVersion,
          environment,
          mainResponsibleId,
          testResponsibleId,
        } = values;
        const { typeCode } = originIssueTypes.find((t) => t.id === typeId);
        // 手动检验描述是否必输校验
        const descriptionField = fields.find((f) => f.fieldCode === 'description');
        if (descriptionField && descriptionField.required && this.checkSameDescription(undefined, description)) {
          Choerodon.prompt('请填写描述');
          return;
        }
        if (typeCode === 'feature' && epicId) {
          const hasSame = await featureApi.hasSameInEpicBySummary(summary, epicId);
          if (hasSame) {
            Choerodon.prompt('史诗下已含有同名特性');
            return;
          }
        }
        const exitComponents = originComponents;
        const componentIssueRelVOList = map(componentIssueRel
          && componentIssueRel.filter((v) => v && v.trim()), (component) => {
          const target = find(exitComponents, { name: component.trim() });
          if (target) {
            return target;
          }
          return ({
            name: component.trim(),
            projectId: getProjectId(),
          });
        });
        const exitLabels = originLabels;
        const labelIssueRelVOList = map(issueLabel, (label) => {
          const target = find(exitLabels, { labelName: label });
          if (target) {
            return target;
          }
          return ({
            labelName: label,
            projectId: getProjectId(),
          });
        });
        const fixVersionIssueRelVOList = map(fixVersionIssueRel, (versionId) => ({
          versionId,
          relationType: 'fix',
        }));
        const issueLinkCreateVOList = this.getIssueLinks(keys, linkTypes, linkIssues);

        this.setState({ createLoading: true });
        const deltaOps = description;
        try {
          const text = await uploadAndReplaceImg(deltaOps);
          const extra = {
            description: text,
            programId: getProjectId(),
            projectId: getProjectId(),
            issueTypeId: typeId,
            typeCode,
            summary: summary.trim(),
            priorityId: priorityId || 0,
            priorityCode: `priority-${priorityId || 0}`,
            sprintId: sprintId || 0,
            epicId: epicId || 0,
            piId: pi || 0,
            epicName,
            parentIssueId: subTaskParent || parentIssueId || 0, // 子任务
            relateIssueId: subBugParent || relateIssueId || 0, // 子bug
            assigneeId: assigneedId,
            labelIssueRelVOList,
            versionIssueRelVOList: fixVersionIssueRelVOList,
            componentIssueRelVOList,
            storyPoints,
            remainingTime: estimatedTime,
            issueLinkCreateVOList,
            featureVO: {
              benfitHypothesis,
              acceptanceCritera,
              featureType,
            },
            wsjfVO: {
              userBusinessValue,
              timeCriticality,
              rrOeValue,
              jobSize,
            },
            featureId, // 特性字段
            teamProjectIds,
            programVersion,
            environment, // 缺陷有的字段
            mainResponsibleId,
            testResponsibleId,
            estimatedEndTime: estimatedEndTime && estimatedEndTime.format('YYYY-MM-DD HH:mm:ss'),
            estimatedStartTime: estimatedStartTime && estimatedStartTime.format('YYYY-MM-DD HH:mm:ss'),
          };
          this.handleSave(extra, fileList);
        } catch (error) {
          console.log(error);
          this.setState({ createLoading: false });
        }
      }
    });
  };

  handleCancel = () => {
    const { onCancel, form } = this.props;
    form.resetFields();
    this.setState({
      createLoading: false,
    });
    if (onCancel) {
      onCancel();
    }
  };

  // 分派给我
  assigneeMe = () => {
    const { id } = AppState.userInfo;
    const { form } = this.props;
    form.setFieldsValue({
      assigneedId: id,
    });
  };

  getIssueTypes = (isInProgram) => {
    const { mode } = this.props;
    const { originIssueTypes } = this.state;
    // const filterSubType = (type) => (!['sub_task'].includes(type.typeCode));
    const filterEpic = (type) => (!['issue_epic'].includes(type.typeCode));
    const filterFeature = (type) => (!['feature'].includes(type.typeCode));
    const issueTypes = applyFilter(originIssueTypes, [
      // filterSubType,
      {
        filter: filterEpic,
        apply: isInProgram || mode === 'feature', // 在项目群下的子项目和创建feature时，把epic过滤掉
      }, {
        filter: filterFeature,
        apply: mode !== 'program', // 在项目群中创建issue时不过滤feature类型
      }]);
    return issueTypes;
  }

  setDefaultSelect = (field) => (list, defaultValue) => {
    const { form } = this.props;
    form.setFieldsValue({
      [field]: defaultValue,
    });
  }

  checkEpicNameRepeat = (rule, value, callback) => {
    if (value && value.trim()) {
      epicApi.checkName(value)
        .then((res) => {
          if (res) {
            callback('史诗名称重复');
          } else {
            callback();
          }
        });
    } else {
      callback();
    }
  };

  getFieldComponent = (field) => {
    const {
      form, mode, hiddenIssueType, teamProjectIds,
    } = this.props;
    const { getFieldDecorator } = form;
    const {
      defaultValue, fieldName, fieldCode, fieldType, required,
    } = field;
    const {
      originIssueTypes,
      newIssueTypeCode, defaultTypeId,
    } = this.state;

    switch (field.fieldCode) {
      case 'issueType':
        return (
          [
            ['sub_task', 'sub_bug', 'feature'].includes(mode) || hiddenIssueType
              ? getFieldDecorator('typeId', {
                rules: [{ required: true, message: '问题类型为必输项' }], // 不需要展示，但是要有值
                initialValue: defaultTypeId || '',
              })
              : (
                <IsInProgram>
                  {
                    ({ isInProgram }) => (
                      <FormItem label="问题类型">
                        {getFieldDecorator('typeId', {
                          rules: [{ required: true, message: '问题类型为必输项' }],
                          initialValue: defaultTypeId || '',
                        })(<Select
                          label="问题类型"
                          getPopupContainer={(triggerNode) => triggerNode.parentNode}
                          onChange={((value) => {
                            const { typeCode } = originIssueTypes.find(
                              (item) => item.id === value,
                            );
                            const param = {
                              schemeCode: 'agile_issue',
                              context: typeCode,
                              pageCode: 'agile_issue_create',
                            };
                            fieldApi.getFields(param).then((res) => {
                              const { fields } = this.state;
                              form.resetFields(['assigneedId', 'sprintId', 'priorityId', 'epicId', 'componentIssueRel',
                                'estimatedTime', 'storyPoints', 'fixVersionIssueRel', 'issueLabel',
                                ...fields.map((f) => f.fieldCode).filter((code) => !['typeId', 'summary', 'description'].some((i) => i === code))]);
                              this.setState({
                                fields: res,
                                newIssueTypeCode: typeCode,
                              });
                              this.loadDefaultTemplate(typeCode);
                              this.setDefaultValue(res);
                            });
                          })}
                        >
                          {this.getIssueTypes(isInProgram).map((type) => (
                            <Option key={type.id} value={type.id}>
                              <TypeTag
                                data={type}
                                showName
                              />
                            </Option>
                          ))}
                        </Select>)}
                      </FormItem>
                    )
                  }
                </IsInProgram>
              ),
            newIssueTypeCode === 'sub_task' && !['sub_task', 'sub_bug', 'feature'].includes(mode) ? (
              <FormItem>
                {getFieldDecorator('subTaskParent', {
                  rules: [{ required: true, message: '父级任务为必选项' }],
                })(
                  <SelectFocusLoad
                    label="关联父级任务"
                    type="subTask_parent_issue"
                    allowClear
                    onChange={((value) => {
                      this.autoSetSprint(value);
                    })}
                  />,
                )}
              </FormItem>
            ) : null,
            newIssueTypeCode === 'bug' && !['sub_task', 'sub_bug', 'feature'].includes(mode) ? (
              <FormItem>
                {getFieldDecorator('subBugParent', {
                })(
                  <SelectFocusLoad
                    label="关联父级任务"
                    type="subBug_parent_issue"
                    allowClear
                    onChange={((value) => {
                      this.autoSetSprint(value);
                    })}
                  />,
                )}
              </FormItem>
            ) : null,
            newIssueTypeCode === 'feature' ? (
              <FormItem>
                {getFieldDecorator('featureType', {
                  rules: [{ required: true, message: '特性类型为必输项' }],
                  initialValue: 'business',
                })(
                  <Select
                    label="特性类型"
                    getPopupContainer={(triggerNode) => triggerNode.parentNode}
                  >
                    {[
                      {
                        ...this.getDefaultType(),
                        colour: '#3D5AFE',
                        featureType: 'business',
                        name: '特性',
                      }, {
                        ...this.getDefaultType(),
                        colour: '#FFCA28',
                        featureType: 'enabler',
                        name: '使能',
                      },
                    ].map((type) => (
                      <Option key={type.featureType} value={type.featureType}>
                        <TypeTag
                          data={type}
                          showName
                          featureType={type.featureType}
                        />
                      </Option>
                    ))}
                  </Select>,
                )}
              </FormItem>
            ) : null]
        );
      case 'assignee':
        return (
          <FormItem label="经办人" key={`${newIssueTypeCode}-assignee`}>
            <div style={{ display: 'flex', alignItems: 'center' }}>
              {getFieldDecorator('assigneedId', {
                rules: [{ required: field.required, message: '请选择经办人' }],
                initialValue: this.props.chosenAssignee,
              })(
                <SelectFocusLoad
                  type="user"
                  label="经办人"
                  style={{ flex: 1 }}
                  loadWhenMount
                  getPopupContainer={(triggerNode) => triggerNode.parentNode}
                  allowClear
                />,
              )}
              <IsProjectMember>
                {(isProjectMember) => isProjectMember && (
                  <span
                    onClick={this.assigneeMe}
                    role="none"
                    style={{
                      display: 'inline-block',
                      color: 'rgba(63, 81, 181)',
                      marginLeft: 10,
                      cursor: 'pointer',
                    }}
                  >
                    分配给我
                  </span>
                )}
              </IsProjectMember>
            </div>
          </FormItem>

        );
      case 'sprint':
        return (
          <FormItem label="冲刺" key={`${newIssueTypeCode}-sprint`}>
            {getFieldDecorator('sprintId', {
              rules: [{ required: field.required, message: '请选择冲刺' }],
            })(
              <SelectFocusLoad
                label="冲刺"
                allowClear
                type="sprint"
                disabled={['sub_task', 'sub_bug'].includes(mode) || newIssueTypeCode === 'sub_task' || (newIssueTypeCode === 'bug' && form.getFieldValue('subBugParent'))}
                afterLoad={(sprints) => {
                  this.props.chosenSprint && form.setFieldsValue({
                    sprintId: this.props.chosenSprint,
                  });
                }}
              />,
            )}
          </FormItem>
        );
      case 'priority':
        return (
          <FormItem label="优先级">
            {getFieldDecorator('priorityId', {
              rules: [{ required: true, message: '优先级为必选项' }],
            })(
              <SelectFocusLoad
                label="优先级"
                type="priority"
                afterLoad={this.setDefaultSelect('priorityId')}
              />,
            )}
          </FormItem>
        );
      case 'label':
        return (
          <FormItem label="标签">
            {getFieldDecorator('issueLabel', {
              rules: [{
                transform: (value) => (value ? value.toString() : value),
              },
              { required: field.required, message: '请选择标签' },
              ],
              normalize: (value) => (value ? value.map((s) => s.toString().substr(0, 10))
                : value), // 限制最长10位
            })(
              <SelectFocusLoad
                label="标签"
                mode="tags"
                loadWhenMount
                type="label"
              >
                {field.defaultValueObjs?.map((label) => (
                  <Option key={label.labelName} value={label.labelName}>
                    {label.labelName}
                  </Option>
                ))}
              </SelectFocusLoad>,
            )}
          </FormItem>
        );
      case 'feature':
        // 如果在项目群中则不显示史诗 目前 工作列表这边创建问题 不调用这个case
        return (
          <FormItem label="特性">
            {getFieldDecorator('feature', {
              rules: [{ required: field.required, message: '请选择特性' }],
            })(
              <SelectFocusLoad
                label="特性"
                allowClear
                type="feature"
                loadWhenMount
              />,
            )}
          </FormItem>
        );
      case 'fixVersion':
        return (
          <FormItem label="修复的版本">
            {getFieldDecorator('fixVersionIssueRel', {
              rules: [{ transform: (value) => (value ? value.toString() : value) },
                { required: field.required, message: '请选择修复的版本' }],
            })(
              <SelectFocusLoad
                label="修复的版本"
                mode="multiple"
                loadWhenMount
                type="version"
                afterLoad={() => {
                  if (this.props.chosenVersion) {
                    form.setFieldsValue({
                      fixVersionIssueRel: [this.props.chosenVersion],
                    });
                  }
                }}
              />,
            )}
          </FormItem>
        );
      case 'epic':
        return (
          <IsInProgram>
            {
              ({ isInProgram, isShowFeature }) => {
                // 如果在项目群中则不显示史诗
                if (!isInProgram) {
                  return (
                    ['issue_epic', 'sub_task'].includes(newIssueTypeCode) ? null : (
                      <FormItem label="史诗" key="epicId">
                        {getFieldDecorator('epicId', {
                          rules: [{ required: field.required, message: '请选择史诗' }],
                        })(
                          <SelectFocusLoad
                            label="史诗"
                            allowClear
                            type="epic"
                            loadWhenMount
                            afterLoad={() => {
                              form.setFieldsValue({
                                // eslint-disable-next-line react/destructuring-assignment
                                epicId: this.props.epicId,
                              });
                            }}
                          />,
                        )}
                      </FormItem>
                    )
                  );
                } if (isShowFeature && newIssueTypeCode === 'story') {
                  return (
                    <FormItem label="特性" key="featureId">
                      {getFieldDecorator('featureId', {})(
                        <SelectFocusLoad
                          label="特性"
                          allowClear
                          type="feature"
                          loadWhenMount
                          selectedFeature={{
                            issueId: this.props.chosenFeature,
                            summary: this.props.chosenFeatureName,
                          }}
                          afterLoad={() => {
                            form.setFieldsValue({
                              featureId: this.props.chosenFeature,
                            });
                          }}
                        />,
                      )}
                    </FormItem>
                  );
                }
                return '';
              }
            }
          </IsInProgram>
        );
      case 'component':
        return (
          ['sub_task'].includes(newIssueTypeCode) ? null : (
            <FormItem label="模块">
              {getFieldDecorator('componentIssueRel', {
                rules: [{ transform: (value) => (value ? value.toString() : value) },
                  { required: field.required, message: '请选择模块' },
                ],
              })(
                <SelectFocusLoad
                  label="模块"
                  mode="multiple"
                  type="component"
                  allowClear
                >
                  {field.defaultValueObjs?.map((component) => (
                    <Option
                      key={component.name}
                      value={component.name}
                      name={component.name}
                    >
                      {[...component.name].length > 10 ? (
                        <Tooltip title={component.name} placement="top" arrowPointAtCenter>
                          <span style={{ whiteSpace: 'nowrap' }}>
                            {component.name.substring(0, 10)}
                            ...
                          </span>
                        </Tooltip>
                      ) : <span>{component.name}</span>}

                    </Option>
                  ))}
                </SelectFocusLoad>,
              )}
            </FormItem>
          )
        );
      case 'summary':
        return (
          // 切换类型时将组件卸载，保证切换到史诗时的聚焦生效
          <FormItem label="概要" className="c7nagile-line" key={`${newIssueTypeCode}-summary`}>
            {getFieldDecorator('summary', {
              rules: [{ required: true, message: '概要为必输项', whitespace: true }],
            })(
              <DebounceInput autoFocus={newIssueTypeCode !== 'issue_epic'} label="概要" maxLength={44} />,
            )}
          </FormItem>
        );
      case 'epicName':
        return (
          newIssueTypeCode === 'issue_epic' && (
            <FormItem label="史诗名称" className="c7nagile-line">
              {getFieldDecorator('epicName', {
                rules: [{ required: true, message: '史诗名称为必输项' }, {
                  validator: this.checkEpicNameRepeat,
                }],
              })(
                <DebounceInput autoFocus label="史诗名称" maxLength={20} />,
              )}
            </FormItem>
          )
        );
      case 'remainingTime':
        return (
          newIssueTypeCode !== 'issue_epic' && (
            <FormItem>
              {getFieldDecorator('estimatedTime', {
                rules: [{
                  required: field.required,
                  message: '请选择预估时间',
                }, {
                  pattern: /(^\d{1,3}\.{1}\d{1}$)|(^[1-9]\d{0,2}$)/,
                  message: '请输入小于3位的整数或者整数位小于3位小数点后一位的小数',
                }],
              })(
                <SelectNumber
                  label="预估时间"
                  loose
                  getPopupContainer={(triggerNode) => triggerNode.parentNode}
                />,
              )}
            </FormItem>
          )
        );
      case 'storyPoints':
        return (
          newIssueTypeCode === 'story' && (
            <FormItem>
              {getFieldDecorator('storyPoints', {
                rules: [{
                  required: field.required,
                  message: '请填写故事点',
                }, {
                  pattern: /(^\d{1,3}\.{1}\d{1}$)|(^[1-9]\d{0,2}$)/,
                  message: '请输入小于3位的整数或者整数位小于3位小数点后一位的小数',
                }],
              })(
                <SelectNumber
                  label="故事点"
                  loose
                  getPopupContainer={(triggerNode) => triggerNode.parentNode}
                />,
              )}
            </FormItem>
          )
        );
      case 'description':
        return (
          <>
            <FormItem key={newIssueTypeCode} label={fieldName} className="c7nagile-line" required={field.required}>
              {getFieldDecorator(fieldCode, {
                initialValue: this.props.defaultDescription,
              })(
                <DebounceEditor
                  style={{ height: 200, width: '100%' }}
                />,
              )}
            </FormItem>
            <FormItem className="c7nagile-line">
              {getFieldDecorator('fileList', {
                valuePropName: 'fileList',
                getValueFromEvent: normFile,
                rules: [{
                  validator: validateFile,
                }],
              })(
                <UploadButton />,
              )}
            </FormItem>
          </>
        );
      case 'benfitHypothesis':
        return (
          <FormItem key={field.id}>
            {getFieldDecorator('benfitHypothesis', {
              rules: [{ required: field.required, message: '请填写预估价值' }],
            })(
              <DebounceInput label="特性价值" maxLength={100} />,
            )}
          </FormItem>
        );
      case 'acceptanceCritera':
        return (
          <FormItem key={field.id}>
            {getFieldDecorator('acceptanceCritera', {
              rules: [{ required: field.required, message: '请填写验收标准' }],
            })(
              <DebounceInput label="验收标准" maxLength={100} />,
            )}
          </FormItem>
        );
      case 'pi':
        return (
          <Permission service={[
            'choerodon.code.project.plan.feature.ps.choerodon.code.project.plan.feature.completepi',
            'choerodon.code.project.plan.feature.ps.choerodon.code.project.plan.feature.startpi',
            'choerodon.code.project.plan.feature.ps.pi.plan',
          ]}
          >
            {(hasPermission) => (
              <FormItem key={field.id} label="PI">
                {getFieldDecorator('pi', {
                  rules: [{ required: field.required, message: '请选择pi' }],
                })(
                  <SelectFocusLoad
                    label="PI"
                    type="pi"
                    optionArgs={!hasPermission}
                  >
                    {field.defaultValueObjs?.map((pi) => (
                      <Option
                        key={pi.id}
                        value={pi.id}
                      >
                        {`${pi.code}-${pi.name}`}
                      </Option>
                    ))}
                  </SelectFocusLoad>,
                )}
              </FormItem>
            )}
          </Permission>

        );
      case 'estimatedStartTime':
        return (
          <FieldStartTime
            form={form}
            field={field || {}}
          />
        );
      case 'estimatedEndTime':
        return (
          <FieldEndTime
            form={form}
            field={field || {}}
          />
        );
      case 'subProject':
        return newIssueTypeCode === 'feature'
          && <FieldTeam form={form} teamProjectIds={teamProjectIds} field={field || {}} />;
      case 'programVersion':
        return (
          newIssueTypeCode === 'feature' && (
            <FormItem key={field.id} label="版本233">
              {getFieldDecorator('programVersion', {
                rules: [{ required: field.required, message: '版本为必输项' }],
              })(
                <SelectProgramVersion mode="multiple" placeholder="版本" />,
              )}
            </FormItem>
          )
        );
      case 'environment':
        return (
          <FormItem key={field.id} label="环境">
            {getFieldDecorator('environment', {
              rules: [{ required: field.required, message: '环境为必输项' }],
            })(
              <SelectEnvironment placeholder="环境" />,
            )}
          </FormItem>
        );
      case 'testResponsible':
      case 'mainResponsible':
        return (
          <FormItem label={field.fieldName} key={`${newIssueTypeCode}-${field.id}`}>
            <div style={{ display: 'flex', alignItems: 'center' }}>
              {getFieldDecorator(`${field.fieldCode}Id`, {
                rules: [{ required: field.required, message: `请选择${field.fieldName}` }],
              })(
                <SelectFocusLoad
                  type="user"
                  label={field.fieldName}
                  style={{ flex: 1 }}
                  loadWhenMount
                  getPopupContainer={(triggerNode) => triggerNode.parentNode}
                  allowClear
                />,
              )}
            </div>
          </FormItem>

        );

      default:
        return (
          <FormItem label={fieldName} style={{ width: 330 }}>
            {getFieldDecorator(fieldCode, {
              rules: [{ required, message: `${fieldName}为必填项` }],
              getValueFromEvent: fieldType === 'number' ? (value) => (value ? String(value) : undefined) : undefined,
              initialValue: this.transformValue(fieldType, defaultValue),
            })(
              renderField(field),
            )}
          </FormItem>
        );
    }
  };

  transformValue = (fieldType, value) => {
    if (value) {
      if (fieldType === 'time' || fieldType === 'datetime' || fieldType === 'date') {
        return value ? moment(value) : undefined;
      } if (value instanceof Array) {
        return value.slice();
      }
      return value;
    }
    return undefined;
  };

  renderIssueLinks = () => {
    const { newIssueTypeCode } = this.state;
    const { form: { getFieldDecorator, getFieldValue } } = this.props;
    getFieldDecorator('keys', { initialValue: [] });
    const keys = getFieldValue('keys');
    if (newIssueTypeCode !== 'issue_epic') {
      return keys.map((k, index) => (
        <div style={{ display: 'flex' }} key={k}>
          <div style={{ flex: 1, display: 'flex' }}>
            <FormItem label="关系" style={{ width: '30%' }}>
              {getFieldDecorator(`linkTypes[${index}]`, {
              })(
                <SelectFocusLoad
                  label="关系"
                  type="issue_link"
                />,
              )}
            </FormItem>
            <FormItem label="问题" style={{ marginLeft: 20, width: 'calc(70% - 20px)' }}>
              {getFieldDecorator(`linkIssues[${index}]`, {
              })(
                <SelectFocusLoad
                  label="问题"
                  type="issues_in_link"
                />,
              )}
            </FormItem>
          </div>
          <div style={{ marginTop: 10, width: 70, marginLeft: 20 }}>
            <Button
              shape="circle"
              icon="add"
              onClick={this.add}
            />
            {
              keys.length > 1 ? (
                <Button
                  shape="circle"
                  style={{ marginLeft: 10 }}
                  icon="delete"
                  onClick={() => this.remove(k)}
                />
              ) : null
            }
          </div>
        </div>
      ));
    }
    return null;
  }

  render() {
    const {
      visible, form, parentSummary, title, mode,
      contentTitle,
      // contentDescription,
      // contentLink,
      hiddenFields,
    } = this.props;
    const {
      createLoading, fields, loading, newIssueTypeCode,
    } = this.state;

    return (
      <Sidebar
        className="c7n-createIssue"
        title={title}
        visible={visible && !loading}
        onOk={this.handleCreateIssue}
        onCancel={this.handleCancel}
        okText="创建"
        cancelText="取消"
        confirmLoading={createLoading}
        width={MODAL_WIDTH.middle}
        maskClosable={false}
        keyboard={false}
      >
        <Content>
          <Spin spinning={loading}>
            <Form layout="vertical" style={{ width: 670 }} className="c7nagile-form">
              <div className="c7nagile-createIssue-fields" key={newIssueTypeCode}>
                {['sub_task', 'sub_bug'].includes(mode) && (
                  <FormItem>
                    <Input label="父任务概要" value={parentSummary} disabled />
                  </FormItem>
                )}
                {fields && fields.filter((field) => !hiddenFields.includes(field.fieldCode))
                  .map((field) => <span key={field.id}>{this.getFieldComponent(field)}</span>)}
                {newIssueTypeCode === 'feature' && <WSJF getFieldDecorator={form.getFieldDecorator} />}
              </div>
              {mode !== 'feature' && mode !== 'sub_task' && !['sub_task', 'issue_epic', 'feature'].includes(newIssueTypeCode) && <FieldIssueLinks form={form} />}
            </Form>
          </Spin>
        </Content>
      </Sidebar>
    );
  }
}
CreateIssue.defaultProps = defaultProps;
export default Form.create({})(observer(CreateIssue));
