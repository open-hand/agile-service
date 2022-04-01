import React, {
  useMemo, useCallback, useState, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Form, Select, DataSet, TextField, TextArea, IconPicker, Modal, Button, CheckBox,
  Icon,
} from 'choerodon-ui/pro';
import { debounce } from 'lodash';
import { CompactPicker } from 'react-color';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Choerodon } from '@choerodon/boot';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { issueTypeApi, IUpdate, ICreate } from '@/api';
import { TypeTag } from '@/components';
import { IModalProps } from '@/common/types';
import LINK_URL from '@/constants/LINK_URL';
import to from '@/utils/to';
import styles from './AddIssueType.less';
import useFormatMessage from '@/hooks/useFormatMessage';

interface Props {
  modal?: IModalProps,
  typeId?: string
  typeTableDataSet: DataSet
  isOrganization: boolean,
}

const AddIssueType: React.FC<Props> = ({
  modal, typeId, typeTableDataSet, isOrganization,
}) => {
  const [addLoading, setAddLoading] = useState<boolean>(false);
  const [editLoading, setEditLoading] = useState<boolean>(false);
  const [colour, setColor] = useState<string>('#5365EA');
  const [pickerDisplay, setPickerDisplay] = useState<boolean>(false);
  const [initType, setInitType] = useState<any>();
  const isSystemType = initType?.source === 'system';
  const formatMessage = useFormatMessage();
  const checkName = useCallback(async (value) => {
    if (typeId && value === initType?.name) {
      return true;
    }
    const res = isOrganization ? await issueTypeApi.orgCheckName(value, typeId) : await issueTypeApi.checkName(value, typeId);
    if (res) {
      return '名称重复';
    }
    return true;
  }, [initType?.name, isOrganization, typeId]);

  const checkIcon = useCallback(async (value) => {
    if (typeId && value === initType?.icon) {
      return true;
    }
    const res = isOrganization ? await issueTypeApi.orgCheckIcon(value, typeId) : await issueTypeApi.checkIcon(value, typeId);
    if (res) {
      return '图标重复';
    }
    return true;
  }, [initType?.icon, isOrganization, typeId]);

  const standardTypeDataSet = useMemo(() => new DataSet({
    data: [
      {
        name: '故事',
        code: 'story',
      },
      {
        name: '任务',
        code: 'task',
      },
      {
        name: '缺陷',
        code: 'bug',
      },
      {
        name: '子任务',
        code: 'sub_task',
      },
    ],
    fields: [{
      name: 'name',
      type: 'string' as FieldType,
    }, {
      name: 'code',
      type: 'string' as FieldType,
    }],
  }), []);

  const addDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'name',
        type: 'string' as FieldType,
        label: formatMessage({ id: 'agile.issueType.name' }),
        required: true,
        maxLength: 6,
        validator: checkName,
      },
      {
        name: 'description',
        type: 'string' as FieldType,
        label: formatMessage({ id: 'agile.issueType.description' }),
        maxLength: 255,
      },
      {
        name: 'typeCode',
        type: 'string' as FieldType,
        textField: 'name',
        valueField: 'code',
        label: formatMessage({ id: 'agile.issueType.norm.type' }),
        required: !isSystemType,
        disabled: !!typeId,
        options: standardTypeDataSet,
      },
      {
        name: 'icon',
        type: 'string' as FieldType,
        label: '选择图标',
        required: true,
        validator: checkIcon,
      },
      {
        name: 'copyStatusMachine',
        label: '是否复制状态机',
      },
      {
        name: 'copyCustomField',
        label: '是否复制自定义字段',
      },
    ],
  }), [checkIcon, checkName, formatMessage, isSystemType, standardTypeDataSet, typeId]);

  const handleChangeColor = useCallback((newColor: any) => {
    setColor(newColor.hex);
  }, []);

  const handleClickSwatch = useCallback(() => {
    if (!(isSystemType && isOrganization)) {
      setPickerDisplay(!pickerDisplay);
    }
  }, [isOrganization, isSystemType, pickerDisplay]);

  const handlePickerHidden = useCallback(() => {
    setPickerDisplay(false);
  }, []);

  const saveEdit = useCallback((data: IUpdate, callback?: Function) => {
    setEditLoading(true);
    // eslint-disable-next-line no-nested-ternary
    issueTypeApi[isOrganization ? 'orgUpdate' : (isSystemType ? 'systemUpdate' : 'update')](typeId as string, data).then((res: any) => {
      Choerodon.prompt('修改成功');
      setEditLoading(false);
      typeTableDataSet.query(typeTableDataSet.currentPage);
      modal?.close();
      if (callback) {
        callback(res.id || typeId);
      }
    }).catch(() => {
      setEditLoading(false);
      Choerodon.prompt('修改失败');
    });
  }, [isOrganization, isSystemType, modal, typeId, typeTableDataSet]);

  const debounceSaveEdit = debounce((data: IUpdate, callback?: Function) => {
    saveEdit(data, callback);
  }, 300);

  const handleSubmit = useCallback(async (callback?: Function) => {
    const validate = await addDataSet.validate();
    if (validate) {
      const name = addDataSet.current?.get('name');
      const description = addDataSet.current?.get('description');
      const typeCode = addDataSet.current?.get('typeCode');
      const icon = addDataSet.current?.get('icon');
      const copyStatusMachine = !typeId ? addDataSet.current?.get('copyStatusMachine') : undefined;
      const copyCustomField = !typeId ? addDataSet.current?.get('copyCustomField') : undefined;

      const data = {
        id: typeId,
        colour,
        name,
        description,
        typeCode,
        icon,
        copyStatusMachine,
        copyCustomField,
      };
      if (!typeId) {
        setAddLoading(true);
        issueTypeApi[isOrganization ? 'orgCreate' : 'create'](data as ICreate).then((res: any) => {
          Choerodon.prompt('创建成功');
          typeTableDataSet.query();
          // setAddLoading(false);
          modal?.close();
          if (callback) {
            callback(res.id);
          }
        }).catch(() => {
          setAddLoading(false);
          Choerodon.prompt('创建失败');
        });
      } else {
        const isModified = initType?.name !== name || initType?.description != description || initType?.icon !== icon || initType?.colour !== colour;
        if (isModified) {
          if (callback) {
            const confirmModal = Modal.open({
              className: styles.confirm_modal,
              style: {
                width: 520,
              },
              key: Modal.key(),
              title: '尚未保存',
              children: (
                <div>您尚未保存当前页面修改的内容，是否保存？</div>
              ),
              okText: '保存',
              cancelText: '不保存',
              okProps: {
                loading: !!editLoading,
              },
              onOk: () => {
                debounceSaveEdit(data as IUpdate, callback);
                confirmModal.close();
              },
              onCancel: () => {
                modal?.close();
                callback(data.id || typeId);
              },
            });
          } else {
            debounceSaveEdit(data as IUpdate);
          }
        } else if (callback) {
          modal?.close();
          callback(data.id);
        }
      }
    }
  }, [addDataSet, colour, debounceSaveEdit, editLoading, initType?.colour, initType?.description, initType?.icon, initType?.name, isOrganization, modal, typeId, typeTableDataSet]);

  const debouncedSubmit = debounce((callback?: Function) => {
    handleSubmit(callback);
  }, 500);

  const handleSave = useCallback(() => {
    debouncedSubmit();
  }, [debouncedSubmit]);

  const handleLinkToPage = useCallback(() => {
    debouncedSubmit((id: string) => {
      to(LINK_URL.pageConfig, {
        type: isOrganization ? 'org' : 'project',
        params: {
          issueTypeId: id,
        },
      });
    });
  }, [debouncedSubmit, isOrganization]);

  const handleLinkToStatus = useCallback(() => {
    debouncedSubmit((id: string) => {
      to(LINK_URL.status, {
        type: isOrganization ? 'org' : 'project',
        params: {
          issueTypeId: id,
        },
      });
    });
  }, [debouncedSubmit, isOrganization]);

  const handleCancel = useCallback(() => {
    modal?.close();
  }, [modal]);

  useEffect(() => {
    if (typeId) {
      issueTypeApi[isOrganization ? 'orgGetType' : 'getType'](typeId).then((res: any) => {
        setInitType(res);
        addDataSet.current?.set('name', res.name);
        addDataSet.current?.set('description', res.description);
        addDataSet.current?.set('icon', res.icon);
        addDataSet.current?.set('typeCode', res.typeCode);
        setColor(res.colour);
      });
    }
  }, [addDataSet, isOrganization, typeId]);
  return (
    <div className={styles.addIssueType}>
      <div className={styles.addIssueType_content}>
        <Form dataSet={addDataSet} disabled={isOrganization && isSystemType} className={styles.addIssueType_form}>
          <TextField name="name" placeholder="请输入工作项类型名称，例如：故事、工单" />
          <TextArea name="description" placeholder="请输入描述，例如：故事是用户的需求，是从用户的角度来描述用户渴望得到的功能。" />
          {
            !isSystemType && (
              <Select name="typeCode" />
            )
          }
          <div className={styles.icon}>
            <IconPicker
              name="icon"
              clearButton={false}
              style={{
                width: '100%',
              }}
            />
            {
              addDataSet.current?.get('icon') && (
              <div className={styles.icon_replace}>
                <TypeTag
                // @ts-ignore
                  data={{
                    colour,
                    icon: addDataSet.current?.get('icon'),
                  }}
                />
              </div>
              )
            }
            <div className={styles.iconPicker}>
              <Icon type="expand_more" />
            </div>
          </div>
          <div className={styles.colorPicker}>
            <div className={`${styles.swatch} ${styles[`swatch_${isOrganization && isSystemType}`]}`} onClick={handleClickSwatch} role="none">
              <div className={styles.color} style={{ background: colour }} />
            </div>
            {
              pickerDisplay
                ? (
                  <div className={styles.popover}>
                    <div className={styles.cover} onClick={handlePickerHidden} role="none" />
                    <CompactPicker color={colour} onChange={handleChangeColor} />
                  </div>
                )
                : null
            }
          </div>
          {
            !typeId && !isOrganization && (
            <CheckBox
              name="copyStatusMachine"
              style={{
                marginTop: -50,
              }}
            />
            )
          }
          {
            !typeId && !isOrganization && (
            <CheckBox
              name="copyCustomField"
              style={{
                marginTop: -70,
              }}
            />
            )
          }
        </Form>
      </div>
      <div className={styles.addIssueType_footer}>
        {
          !typeId ? (
            <>
              <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} loading={addLoading} disabled={addLoading} onClick={handleSave} style={{ marginLeft: 10 }}>保存</Button>
              <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handleLinkToPage} loading={addLoading} disabled={addLoading}>保存并配置页面</Button>
              {
                !isOrganization && (
                  <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handleLinkToStatus} loading={addLoading} disabled={addLoading}>保存并配置状态机</Button>
                )
              }
              <Button funcType={'raised' as FuncType} onClick={handleCancel}>取消</Button>
            </>
          ) : (
            <>
              {
                !(isOrganization && isSystemType) && (
                  <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} loading={editLoading} disabled={editLoading} onClick={handleSave} style={{ marginLeft: 10 }}>保存</Button>
                )
              }
              <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handleLinkToPage} disabled={editLoading} style={{ marginLeft: 10 }}>跳转配置页面</Button>
              {
                !isOrganization && (
                <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} onClick={handleLinkToStatus} disabled={editLoading}>跳转状态机</Button>
                )
              }
              <Button funcType={'raised' as FuncType} onClick={handleCancel}>取消</Button>
            </>
          )
        }
      </div>
    </div>
  );
};

export default observer(AddIssueType);
