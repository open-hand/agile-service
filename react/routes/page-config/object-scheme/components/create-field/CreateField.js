import React, {
  Fragment, useState, useContext, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import _ from 'lodash';
import moment from 'moment';
import {
  Modal,
  Form, Input, Select, TimePicker,
  DatePicker, InputNumber, Checkbox,
} from 'choerodon-ui';
import { Choerodon } from '@choerodon/boot';
import { FormattedMessage } from 'react-intl';
import './CreateField.less';
import * as images from '../../images';
import DragList from '../drag-list';
import Store from '../../stores';
import UserHead from '../../../../../components/UserHead';
import { randomString } from '../../../../../common/utils';

const { Sidebar } = Modal;
const FormItem = Form.Item;
const { TextArea } = Input;
const { Option } = Select;
const singleList = ['radio', 'single'];
const multipleList = ['checkbox', 'multiple'];
const dateList = ['time', 'datetime', 'date'];
// const textList = ['input', 'text', 'url'];
const dateFormat = 'YYYY-MM-DD HH:mm:ss';

const regex = /^[0-9a-zA-Z_]+$/;


function CreateField(props) {
  const context = useContext(Store);
  const [submitting, setSubmitting] = useState(false);
  const [fieldOptions, setFieldOptions] = useState([]);
  // 这两个可以合并
  const [selectLoading, setSelectLoading] = useState(true);
  const [originUsers, setOriginUsers] = useState([]);
  const [isCheck, setIsCheck] = useState(false);
  const [dateDisable, setDateDisable] = useState(false);

  const [field, setField] = useState({
    fieldType: '',
  });

  // const [selectTypeVisible, setSelectTypeVisible] = useState(false);
  const {
    objectSchemeStore, AppState, intl, schemeCode,
  } = context;

  const initCurrentMenuType = () => {
    objectSchemeStore.initCurrentMenuType(AppState.currentMenuType);
  };
  const init = () => {
    initCurrentMenuType();
    Promise.all([
      objectSchemeStore.loadLookupValue('field_type'),
      objectSchemeStore.loadLookupValue('object_scheme_field_context'),
    ]).then(([fieldType, fieldContext]) => {
      objectSchemeStore.initLookupValue(fieldType, fieldContext);
    });
  };

  const handleClose = () => {
    const { form } = props;
    const { onClose } = context;
    form.resetFields();
    onClose();
  };
  const handleOk = () => {
    const { form } = props;
    const { onOk } = context;
    form.resetFields();
    onOk();
    // setReLoad(!reLoad);
  };

  const handleSubmit = () => {
    const {
      form,
    } = props;
    form.validateFieldsAndScroll((err, data) => {
      if (!err) {
        const { type } = AppState.currentMenuType;
        const prefix = type === 'project' ? 'pro_' : 'org_';
        const postData = {
          ...data,
          schemeCode,
          fieldOptions,
          extraConfig: isCheck,
          defaultValue: String(data.defaultValue || ''),
          code: `${prefix}${data.code}`,
        };

        if (singleList.indexOf(field.fieldType) !== -1) {
          if (fieldOptions.length === 0) {
            Choerodon.prompt('字段列表不能为空');
            return;
          }
          postData.fieldOptions = fieldOptions.map((o) => {
            if (data.defaultValue && (o.tempKey === data.defaultValue || o.id === data.defaultValue)) {
              return { ...o, isDefault: true };
            } else {
              return { ...o, isDefault: false };
            }
          });
        } else if (multipleList.indexOf(field.fieldType) !== -1) {
          if (fieldOptions.length === 0) {
            Choerodon.prompt('字段列表不能为空');
            return;
          }
          postData.fieldOptions = fieldOptions.map((o) => {
            if (data.defaultValue.indexOf(String(o.tempKey)) !== -1
              || data.defaultValue.indexOf(String(o.id)) !== -1) {
              return { ...o, isDefault: true };
            } else {
              return { ...o, isDefault: false };
            }
          });
        } else if (dateList.indexOf(field.fieldType) !== -1) {
          postData.defaultValue = (data.defaultValue && data.defaultValue.format(dateFormat)) || '';
          if (data.check) {
            postData.defaultValue = moment().format(dateFormat);
          }
        } else if (field.fieldType === 'number') {
          postData.defaultValue = data.defaultValue === 0 || data.defaultValue ? String(data.defaultValue) : '';
        }
        setSubmitting(true);
        objectSchemeStore.createField(postData)
          .then((res) => {
            if (!res.failed) {
              Choerodon.prompt(intl.formatMessage({ id: 'createSuccess' }));
              handleOk();
            } else {
              Choerodon.prompt(intl.formatMessage({ id: 'createFailed' }));
              setSubmitting(false);
            }
            //  catch 抛出异常   疑似 Store中createField出错， promise的待验证是否有reject
          }).catch((error) => {
            Choerodon.prompt(intl.formatMessage({ id: 'createFailed' }));
            setSubmitting(false);
          });
      }
    });
  };

  const onTreeChange = (newFieldOptions) => {
    setFieldOptions(newFieldOptions);
  };

  const onTreeCreate = (code, value) => {
    setFieldOptions([...fieldOptions, {
      enabled: true,
      status: 'add',
      code,
      value,
      tempKey: randomString(5),
    }]);
  };

  const onTreeDelete = (tempKey) => {
    const { form } = props;
    const newDefaultValue = form.getFieldValue('defaultValue');
    if (multipleList.indexOf(field.fieldType) !== -1) {
      const newValue = newDefaultValue.filter(v => v !== String(tempKey));
      form.setFieldsValue({ defaultValue: newValue });
    } else if (singleList.indexOf(field.fieldType) !== -1) {
      if (newDefaultValue === tempKey) {
        form.setFieldsValue({ defaultValue: '' });
      }
    }
  };

  const checkName = (rule, value, callback) => {
    if (!value) {
      callback();
    } else {
      objectSchemeStore.checkName(value, schemeCode)
        .then((data) => {
          if (data) {
            callback(intl.formatMessage({ id: 'field.name.exist' }));
          } else {
            callback();
          }
        }).catch(() => {
          callback();
        });
    }
  };

  const checkCode = (rule, value, callback) => {
    if (!value) {
      callback();
    } else if (!regex.test(value)) {
      callback(intl.formatMessage({ id: 'field.code.rule' }));
    } else {
      const { type } = AppState.currentMenuType;
      const prefix = type === 'project' ? 'pro_' : 'org_';
      objectSchemeStore.checkCode(`${prefix}${value}`, schemeCode)
        .then((data) => {
          if (data) {
            callback(intl.formatMessage({ id: 'field.code.exist' }));
          } else {
            callback();
          }
        }).catch(() => {
          callback(intl.formatMessage({ id: 'network.error' }));
        });
    }
  };

  useEffect(() => {
    init();
  }, []);

  const setfieldType = (value) => {
    setField({ fieldType: value });
  };

  const handleSelectChange = (value) => {
    const { form } = props;
    // setSelectTypeVisible(false);
    // console.log('handleSelectChange', value);
    form.setFieldsValue({ defaultValue: '' });
    form.setFieldsValue({ check: false });
    // setDefaultValue([]);
    // setIsCheck(false);
    setDateDisable(false);
    setFieldOptions([]);
    setfieldType(value);
  };


  const handleCheck = (e, type) => {
    const { form } = props;
    if (dateList.indexOf(type) !== -1) {
      form.setFieldsValue({ defaultValue: moment() });
    }

    setIsCheck(e.target.checked);
    setDateDisable(e.target.checked);
  };
  let sign = false;
  const debounceFilterUsers = _.debounce((input) => {
    setSelectLoading(true);
    objectSchemeStore.getUsers(input).then((res) => {
      setOriginUsers(res.list.filter(u => u.enabled));
      setSelectLoading(false);
    });
  }, 500);
  function onFilterChangeAssignee(input) {
    if (!sign) {
      setSelectLoading(true);
      objectSchemeStore.getUsers(input).then((res) => {
        setOriginUsers(res.list.filter(u => u.enabled));
        setSelectLoading(false);
      });
      sign = true;
    } else {
      debounceFilterUsers(input);
    }
  }
  const render = () => {
    const { form } = props;
    const { getFieldDecorator, getFieldValue } = form;
    const {
      visible,
    } = context;

    const fieldType = objectSchemeStore.getFieldType;
    const fieldContext = objectSchemeStore.getFieldContext;

    const selectedContext = getFieldValue('context') || [];
    return (
      <Sidebar
        title={<FormattedMessage id="field.create" />}
        visible={visible}
        onOk={handleSubmit}
        onCancel={handleClose}
        okText={<FormattedMessage id="save" />}
        cancelText={<FormattedMessage id="cancel" />}
        // className="issue-field-sildebar"
        width={740}
        confirmLoading={submitting}
      >
        <div className="issue-region">
          <Form
            // style={{ width: 512 }}
            layout="vertical"
            onSubmit={handleOk}
            className="c7n-sidebar-form c7nagile-form"
          >
            <FormItem>
              {getFieldDecorator('code', {
                rules: [{
                  required: true,
                  whitespace: true,
                  message: '字段编码为必填项！',
                }, {
                  validator: checkCode,
                }],
              })(
                <Input
                  maxLength={10}
                  label={<FormattedMessage id="code" />}
                />,
              )}
            </FormItem>
            <FormItem>
              {getFieldDecorator('name', {
                rules: [{
                  required: true,
                  whitespace: true,
                  message: '字段名称为必填项！',
                }, {
                  validator: checkName,
                }],
              })(
                <Input
                  maxLength={6}
                  label={<FormattedMessage id="name" />}
                />,
              )}
            </FormItem>
            <FormItem>
              {getFieldDecorator('fieldType', {
                rules: [{
                  required: true,
                  message: '字段类型为必填项！',
                }],
              })(
                <Select
                  label={<FormattedMessage id="field.type" />}
                  dropdownMatchSelectWidth
                  size="default"
                  optionLabelProp="name"
                  onChange={handleSelectChange}
                >
                  {fieldType.map(type => (
                    <Option
                      value={type.valueCode}
                      key={type.valueCode}
                      name={intl.formatMessage({ id: `field.${type.valueCode}` })}
                    >
                      <img src={images[type.valueCode]} alt="" className="issue-field-img" />
                      <span>
                        <FormattedMessage id={`field.${type.valueCode}`} />
                      </span>
                    </Option>
                  ))}
                </Select>,
              )}
            </FormItem>
            <FormItem>
              {getFieldDecorator('context', {
                rules: [{
                  required: true,
                  message: '问题类型为必填项！',
                }],
              })(
                <Select
                  label={<FormattedMessage id="field.context" />}
                  dropdownMatchSelectWidth
                  showCheckAll={false}
                  size="default"
                  mode="multiple"
                >
                  {fieldContext.map(ctx => (
                    <Option
                      disabled={ctx.valueCode === 'global' ? selectedContext.length > 0 && !selectedContext.includes('global') : selectedContext.includes('global')}
                      value={ctx.valueCode}
                      key={ctx.valueCode}
                    >
                      {ctx.name}
                    </Option>
                  ))}
                </Select>,
              )}
            </FormItem>

            {/* {
              field.fieldType === 'radio'
                ? (
                  <Fragment>
                    <FormItem>
                      {getFieldDecorator('defaultValue', {
                        rules: [{ required: field.required, message: '必填字段默认值不能为空！' }],
                      })(
                        <Select
                          label={<FormattedMessage id="field.default" />}
                          dropdownMatchSelectWidth
                          notFoundContent={intl.formatMessage({ id: 'field.value.null' })}
                          allowClear
                        >
                          {fieldOptions && fieldOptions.length > 0
                            && fieldOptions.map((item) => {
                              if (item.enabled) {
                                return (
                                  <Option
                                    value={item.tempKey || item.id}
                                    key={item.tempKey || item.id}
                                  >
                                    {item.value}
                                  </Option>
                                );
                              }
                              return [];
                            })}
                        </Select>,
                      )}
                    </FormItem>
                    <DragList
                      title={intl.formatMessage({ id: `field.${field.fieldType}` })}
                      data={fieldOptions}
                      tips={intl.formatMessage({ id: 'field.dragList.tips' })}
                      onChange={onTreeChange}
                      onCreate={onTreeCreate}
                      onDelete={onTreeDelete}
                      onInvalid={onTreeDelete}
                    />
                  </Fragment>
                ) : ''
            }
            {
              field.fieldType === 'checkbox'
                ? (
                  <Fragment>
                    <FormItem>
                      {getFieldDecorator('defaultValue', {
                        rules: [{ required: field.required, message: '必填字段默认值不能为空！' }],
                      })(
                        <Select
                          label={<FormattedMessage id="field.default" />}
                          dropdownMatchSelectWidth
                          mode="multiple"

                          notFoundContent={intl.formatMessage({ id: 'field.value.null' })}
                        >
                          {fieldOptions && fieldOptions.length > 0
                            && fieldOptions.map((item) => {
                              if (item.enabled) {
                                return (
                                  <Option
                                    value={item.tempKey || String(item.id)}
                                    key={item.tempKey || String(item.id)}
                                  >
                                    {item.value}
                                  </Option>
                                );
                              }
                              return [];
                            })}
                        </Select>,
                      )}
                    </FormItem>
                    <DragList
                      title={intl.formatMessage({ id: `field.${field.fieldType}` })}
                      data={fieldOptions}
                      tips={intl.formatMessage({ id: 'field.dragList.tips' })}
                      onChange={onTreeChange}
                      onCreate={onTreeCreate}
                      onDelete={onTreeDelete}
                      onInvalid={onTreeDelete}
                    />
                  </Fragment>
                ) : ''
            } */}
            {
              field.fieldType === 'time'
                ? (
                  <Fragment>
                    <FormItem>
                      {getFieldDecorator('defaultValue', {
                        rules: [{ required: field.required && !dateDisable, message: '必填字段默认值不能为空！' }],
                      })(
                        <TimePicker
                          label={<FormattedMessage id="field.default" />}
                          defaultOpenValue={moment('00:00:00', 'HH:mm:ss')}
                          style={{ width: '100%' }}
                          disabled={dateDisable}
                          allowEmpty
                        />,
                      )}
                    </FormItem>
                    <FormItem>
                      {getFieldDecorator('check', {
                        valuePropName: 'checked',
                        initialValue: false,
                      })(
                        <Checkbox onChange={e => handleCheck(e, 'time')}>
                          <FormattedMessage id="field.useCurrentTime" />
                        </Checkbox>,
                      )}
                    </FormItem>
                  </Fragment>
                ) : ''
            }
            {
              field.fieldType === 'datetime'
                ? (
                  <Fragment>
                    <FormItem>
                      {getFieldDecorator('defaultValue', {
                        rules: [{ required: field.required && !dateDisable, message: '必填字段默认值不能为空！' }],
                      })(
                        <DatePicker
                          label={<FormattedMessage id="field.default" />}
                          format="YYYY-MM-DD HH:mm:ss"
                          showTime={{ defaultValue: moment('00:00:00', 'HH:mm:ss') }}
                          style={{ width: '100%' }}
                          disabled={dateDisable}
                          allowClear
                        />,
                      )}
                    </FormItem>
                    <FormItem>
                      {getFieldDecorator('check', {
                        valuePropName: 'checked',
                        initialValue: false,
                      })(
                        <Checkbox onChange={e => handleCheck(e, 'datetime')}>
                          <FormattedMessage id="field.useCurrentDate" />
                        </Checkbox>,
                      )}
                    </FormItem>
                  </Fragment>
                ) : ''
            }
            {
              field.fieldType === 'date'
                ? (
                  <Fragment>
                    <FormItem>
                      {getFieldDecorator('defaultValue', {
                        rules: [{ required: field.required && !dateDisable, message: '必填字段默认值不能为空！' }],
                      })(
                        <DatePicker
                          label={<FormattedMessage id="field.default" />}
                          format="YYYY-MM-DD"
                          style={{ width: '100%' }}
                          disabled={dateDisable}
                          allowClear
                        />,
                      )}
                    </FormItem>
                    <FormItem>
                      {getFieldDecorator('check', {
                        valuePropName: 'checked',
                        initialValue: false,
                      })(
                        <Checkbox onChange={e => handleCheck(e, 'datetime')}>
                          <FormattedMessage id="field.useCurrentDate" />
                        </Checkbox>,
                      )}
                    </FormItem>
                  </Fragment>
                ) : ''
            }
            {
              field.fieldType === 'number'
                ? (
                  <Fragment>
                    <FormItem>
                      {getFieldDecorator('check', {
                        valuePropName: 'checked',
                      })(
                        <Checkbox onChange={handleCheck}>
                          <FormattedMessage id="field.decimal" />
                        </Checkbox>,
                      )}
                    </FormItem>
                    <FormItem>
                      {getFieldDecorator('defaultValue', {
                        rules: [{ required: field.required, message: '必填字段默认值不能为空！' }],
                      })(
                        <InputNumber
                          step={isCheck ? 0.1 : 1}
                          label={<FormattedMessage id="field.default" />}
                          maxLength={8}
                        />,
                      )}
                    </FormItem>
                  </Fragment>
                ) : ''
            }
            {
              field.fieldType === 'input'
                ? (
                  <FormItem>
                    {getFieldDecorator('defaultValue', {
                      rules: [{ required: field.required, message: '必填字段默认值不能为空！' }],
                    })(
                      <Input
                        label={<FormattedMessage id="field.default" />}
                        maxLength={100}
                      />,
                    )}
                  </FormItem>
                ) : ''
            }
            {
              field.fieldType === 'text'
                ? (
                  <FormItem>
                    {getFieldDecorator('defaultValue', {
                      rules: [{ required: field.required, message: '必填字段默认值不能为空！' }],
                    })(
                      <TextArea
                        label={<FormattedMessage id="field.default" />}
                        maxLength={255}
                      />,
                    )}
                  </FormItem>
                ) : ''
            }
            {
              field.fieldType === 'url'
                ? (
                  <FormItem>
                    {getFieldDecorator('defaultValue', {
                      rules: [{
                        type: 'url',
                        message: intl.formatMessage({ id: 'field.urlError' }),
                      }, {
                        required: field.required,
                        message: '必填字段默认值不能为空！',
                      }],
                    })(
                      <Input
                        label={<FormattedMessage id="field.default" />}
                      />,
                    )}
                  </FormItem>
                ) : ''
            }
            {
              singleList.indexOf(field.fieldType) !== -1
                ? (
                  <Fragment>
                    <FormItem>
                      {getFieldDecorator('defaultValue', {
                        rules: [{ required: field.required, message: '必填字段默认值不能为空！' }],
                      })(
                        <Select
                          label={<FormattedMessage id="field.default" />}
                          dropdownMatchSelectWidth
                          notFoundContent={intl.formatMessage({ id: 'field.value.null' })}
                          allowClear
                        >
                          {fieldOptions && fieldOptions.length > 0
                            && fieldOptions.map((item) => {
                              if (item.enabled) {
                                return (
                                  <Option
                                    value={item.tempKey || item.id}
                                    key={item.tempKey || item.id}
                                  >
                                    {item.value}
                                  </Option>
                                );
                              }
                              return [];
                            })}
                        </Select>,
                      )}
                    </FormItem>
                    <FormItem>
                      <DragList
                        title={intl.formatMessage({ id: `field.${field.fieldType}` })}
                        data={fieldOptions}
                        tips={intl.formatMessage({ id: 'field.dragList.tips' })}
                        onChange={onTreeChange}
                        onCreate={onTreeCreate}
                        onDelete={onTreeDelete}
                        onInvalid={onTreeDelete}
                      />
                    </FormItem>
                  </Fragment>
                ) : ''
            }
            {
              multipleList.indexOf(field.fieldType) !== -1
                ? (
                  <Fragment>
                    <FormItem>
                      {getFieldDecorator('defaultValue', {
                        rules: [{ required: field.required, message: '必填字段默认值不能为空！' }],
                      })(
                        <Select
                          label={<FormattedMessage id="field.default" />}
                          dropdownMatchSelectWidth
                          mode="multiple"
                          notFoundContent={intl.formatMessage({ id: 'field.value.null' })}
                        >
                          {fieldOptions && fieldOptions.length > 0
                            && fieldOptions.map((item) => {
                              if (item.enabled) {
                                return (
                                  <Option
                                    value={item.tempKey || String(item.id)}
                                    key={item.tempKey || String(item.id)}
                                  >
                                    {item.value}
                                  </Option>
                                );
                              }
                              return [];
                            })}
                        </Select>,
                      )}
                    </FormItem>
                    <DragList
                      title={intl.formatMessage({ id: `field.${field.fieldType}` })}
                      data={fieldOptions}
                      tips={intl.formatMessage({ id: 'field.dragList.tips' })}
                      onChange={onTreeChange}
                      onCreate={onTreeCreate}
                      onDelete={onTreeDelete}
                      onInvalid={onTreeDelete}
                    />
                  </Fragment>
                ) : ''
            }
            {
              field.fieldType === 'member'
                ? (
                  <FormItem>
                    {getFieldDecorator('defaultValue', {
                      rules: [{ required: field.required, message: '必填字段默认值不能为空！' }],
                    })(
                      <Select
                        // width="512px"
                        label={<FormattedMessage id="field.default" />}
                        dropdownMatchSelectWidth
                        notFoundContent="没有符合条件的用户"
                        allowClear
                        loading={selectLoading}
                        filter
                        filterOption={false}
                        onFilterChange={onFilterChangeAssignee.bind(this)}
                      >
                        {originUsers.map(user => (
                          <Option key={user.id} value={user.id}>
                            <div style={{ display: 'inline-flex', alignItems: 'center', padding: 2 }}>
                              <UserHead
                                user={{
                                  id: user.id,
                                  loginName: user.loginName,
                                  realName: user.realName,
                                  avatar: user.imageUrl,
                                }}
                              />
                            </div>
                          </Option>
                        ))}
                      </Select>,
                    )}
                  </FormItem>
                ) : ''
            }
            {/* {selectTypeVisible
              ? (
                <DragList
                  title={intl.formatMessage({ id: `field.${field.fieldType}` })}
                  data={fieldOptions}
                  tips={intl.formatMessage({ id: 'field.dragList.tips' })}
                  onChange={onTreeChange}
                  onCreate={onTreeCreate}
                  onDelete={onTreeDelete}
                  onInvalid={onTreeDelete}
                />
              ) : null
            } */}

          </Form>
        </div>
      </Sidebar>
    );
  };
  return render();
}

export default Form.create({})(observer(CreateField));
