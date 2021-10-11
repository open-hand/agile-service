import React, {
  useMemo, useCallback, useRef, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Modal, DataSet, Form, TextField, Button, IconPicker, CheckBox,
} from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import { IModalProps, IIssueType } from '@/common/types';
import { issueTypeApi } from '@/api';
import Field from 'choerodon-ui/pro/lib/data-set/Field';
import { debounce } from 'lodash';
import { ButtonColor, FuncType } from 'choerodon-ui/pro/lib/button/enum';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { TypeTag } from '@/components';
import SelectLinkType from './SelectLinkType';
import styles from './LinkType.less';

interface Props {
  modal?: IModalProps,
  issueTypeDataSet: DataSet,
}

const LinkType: React.FC<Props> = ({ modal, issueTypeDataSet }) => {
  const linkedTypeRef = useRef<IIssueType[]>([]);
  const [renameExist, setRenameExist] = useState<boolean>(false);
  const [newIconExist, setNewIconExist] = useState<boolean>(false);
  const [refrencedLoading, setRerencedLoading] = useState<boolean>(false);

  const checkIcon = useCallback(async (id) => {
    if (id) {
      const typeIcon = (linkedTypeRef.current || []).find((item) => item.id === id)?.icon;
      if (typeIcon) {
        const res = await issueTypeApi.checkIcon(typeIcon);
        if (res) {
          setNewIconExist(true);
          return false;
        }
        setNewIconExist(false);
        return true;
      }
      setNewIconExist(false);
      return true;
    }
    setNewIconExist(false);
    return true;
  }, []);

  const checkName = useCallback(async (id) => {
    if (id) {
      const typeName = (linkedTypeRef.current || []).find((item) => item.id === id)?.name;
      if (typeName) {
        const res = await issueTypeApi.checkName(typeName);
        if (res) {
          setRenameExist(true);
          return false;
        }
        setRenameExist(false);
        return true;
      }
      setRenameExist(false);
      return true;
    }
    setRenameExist(false);
    return true;
  }, []);

  const checkRefrencedType = useCallback(async (id, name, record) => {
    if (id) {
      checkName(id);
      checkIcon(id);
      return true;
    }
    setRenameExist(false);
    setNewIconExist(false);
    return true;
  }, [checkIcon, checkName]);

  const checkRename = useCallback(async (value, name, record) => {
    if (value) {
      const res = await issueTypeApi.checkName(value);
      if (res) {
        return '工作项类型名称重复';
      }
      return true;
    }
    return '请重新设置名称';
  }, []);

  const checkNewIcon = useCallback(async (value) => {
    if (value) {
      const res = await issueTypeApi.checkIcon(value);
      if (res) {
        return '工作项类型图标重复';
      }
      return true;
    }
    return '请重新设置图标';
  }, []);

  const linkDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'id',
      textField: 'name',
      valueField: 'id',
      required: true,
      label: '请选择引用的工作项类型',
      validator: checkRefrencedType,
    }, {
      name: 'copyStatusMachine',
      label: '使用组织预置状态机模板',
      type: 'boolean' as FieldType,
    }, {
      name: 'newName',
      label: '请重新设置名称',
      required: true,
      validator: checkRename,
    }, {
      name: 'newIcon',
      type: 'string' as FieldType,
      label: '请重新设置图标',
      required: true,
      validator: checkNewIcon,
    }],
    events: {
      update: ({
        // @ts-ignore
        // eslint-disable-next-line no-shadow
        dataSet, record, name, value, oldValue,
      }) => {
        if (name === 'id' && !value) {
          dataSet.current.set('newName', undefined);
          dataSet.current.set('newIcon', undefined);
          setRenameExist(false);
          setNewIconExist(false);
        }
      },
    },
  }), [checkNewIcon, checkRefrencedType, checkRename]);

  const handleSubmit = useCallback(async () => {
    let validate = false;
    if (renameExist && newIconExist) {
      validate = await linkDataSet.validate();
    } else {
      const typeIdsField = linkDataSet.current?.getField('id') as Field;
      const checkValidityFieldArr = [typeIdsField];
      if (renameExist) {
        const renameField = linkDataSet.current?.getField('newName') as Field;
        checkValidityFieldArr.push(renameField);
      }

      if (newIconExist) {
        const newIconField = linkDataSet.current?.getField('newIcon') as Field;
        checkValidityFieldArr.push(newIconField);
      }

      await Promise.all(checkValidityFieldArr.map((field) => field?.checkValidity())).then((validateRes) => {
        validate = validateRes.every((res) => !!res);
      });
    }
    if (validate) {
      setRerencedLoading(true);
      const newName = linkDataSet.current?.get('newName');
      const newIcon = linkDataSet.current?.get('newIcon');
      const data = {
        copyStatusMachine: linkDataSet.current?.get('copyStatusMachine'),
      };
      if (newName) {
        Object.assign(data, {
          name: newName,
        });
      }

      if (newIcon) {
        Object.assign(data, {
          icon: newIcon,
        });
      }
      issueTypeApi.referenced(linkDataSet.current?.get('id'), data).then(() => {
        Choerodon.prompt('引用成功');
        setRerencedLoading(false);
        issueTypeDataSet.query();
        modal?.close();
        return true;
      }).catch(() => {
        Choerodon.prompt('引用失败');
        setRerencedLoading(false);
        return false;
      });
    }
    return false;
  }, [issueTypeDataSet, linkDataSet, modal, newIconExist, renameExist]);

  const debouncedSubmit = debounce(() => {
    handleSubmit();
  }, 500);

  const handleSave = useCallback(() => {
    debouncedSubmit();
  }, [debouncedSubmit]);

  const handleCancel = useCallback(() => {
    modal?.close();
  }, [modal]);

  const refrencedType = (linkedTypeRef.current || []).find((item) => item.id === linkDataSet.current?.get('id'));
  const newIcon = linkDataSet.current?.get('newIcon');

  return (
    <div className={styles.linkType}>
      <div className={styles.linkType_content}>
        <Form dataSet={linkDataSet} className={styles.linkType_form}>
          <SelectLinkType name="id" dataRef={linkedTypeRef} />
          <CheckBox name="copyStatusMachine" />
          {
            renameExist && (
              <div style={{
                width: '100%',
                marginTop: -10,
              }}
              >
                <div style={{
                  color: 'rgba(0, 0, 0, 0.64)',
                  marginBottom: 8,
                }}
                >
                  该工作项类型名称和当前项目已有的工作项类型名称重复
                </div>
                <TextField
                  name="newName"
                  style={{
                    width: '100%',
                  }}
                />
              </div>
            )
          }
          {
            newIconExist && (
              <div style={{
                width: '100%',
                marginTop: -10,
              }}
              >
                <div style={{
                  color: 'rgba(0, 0, 0, 0.64)',
                  marginBottom: 8,
                }}
                >
                  该工作项类型名称和当前项目已有的工作项类型图标重复
                </div>
                <div className={styles.icon}>
                  <IconPicker
                    name="newIcon"
                    style={{
                      width: '100%',
                    }}
                  />
                  {
                    refrencedType && refrencedType.colour && newIcon && (
                      <div className={styles.icon_replace}>
                        <TypeTag
                          // @ts-ignore
                          data={{
                            colour: refrencedType.colour,
                            icon: newIcon,
                          }}
                        />
                      </div>
                    )
                  }
                  <div className={styles.iconPicker}>
                    <Icon type="baseline-arrow_drop_down" />
                  </div>
                </div>
              </div>
            )
          }
        </Form>
      </div>
      <div className={styles.linkType_footer}>
        <Button color={'primary' as ButtonColor} funcType={'raised' as FuncType} loading={refrencedLoading} onClick={handleSave} style={{ marginLeft: 10 }}>保存</Button>
        <Button funcType={'raised' as FuncType} onClick={handleCancel}>取消</Button>
      </div>
    </div>

  );
};

const ObserverLinkType = observer(LinkType);

const openLink = (props: Props) => {
  Modal.open({
    className: styles.linkModal,
    drawer: true,
    style: {
      width: 480,
    },
    key: 'link',
    title: '引用工作项类型',
    children: <ObserverLinkType {...props} />,
    footer: null,
  });
};

export default openLink;
