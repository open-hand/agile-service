import React, {
  useMemo, useEffect, useCallback, useRef, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Modal, DataSet, Form, TextField,
} from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/boot';
import { IModalProps, IIssueType } from '@/common/types';
import { issueTypeApi } from '@/api';
import Field from 'choerodon-ui/pro/lib/data-set/Field';
import SelectLinkType from './SelectLinkType';

interface Props {
  modal?: IModalProps,
  issueTypeDataSet: DataSet,
}

const LinkType: React.FC<Props> = ({ modal, issueTypeDataSet }) => {
  const linkedTypeRef = useRef<IIssueType[]>([]);
  const [renameExist, setRenameExist] = useState<boolean>(false);

  const checkName = useCallback(async (id, name, record) => {
    if (id) {
      const typeName = (linkedTypeRef.current || []).find((item) => item.id === id)?.name;
      if (typeName) {
        const res = await issueTypeApi.checkName(typeName);
        if (res) {
          setRenameExist(true);
          return true;
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

  const checkRename = useCallback(async (value, name, record) => {
    if (value) {
      const res = await issueTypeApi.checkName(value);
      if (res) {
        return '问题类型名称重复';
      }
      return true;
    }
    return '请重新设置名称';
  }, []);

  const linkDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'id',
      textField: 'name',
      valueField: 'id',
      required: true,
      label: '请选择引用的问题类型',
      validator: checkName,
    }, {
      name: 'newName',
      label: '请重新设置名称',
      required: true,
      validator: checkRename,
    }],
    events: {
      update: ({
        // @ts-ignore
        // eslint-disable-next-line no-shadow
        dataSet, record, name, value, oldValue,
      }) => {
        if (name === 'id' && !value) {
          dataSet.current.set('newName', undefined);
          setRenameExist(false);
        }
      },
    },
  }), [checkName, checkRename]);

  const handleSubmit = useCallback(async () => {
    let validate = false;
    if (renameExist) {
      validate = await linkDataSet.validate();
    } else {
      const typeIdsField = linkDataSet.current?.getField('id') as Field;
      validate = await typeIdsField.checkValidity();
    }
    if (validate) {
      const newName = linkDataSet.current?.get('newName');
      issueTypeApi.referenced(linkDataSet.current?.get('id'), newName ? { name: newName } : {}).then(() => {
        Choerodon.prompt('引用成功');
        issueTypeDataSet.query();
        modal?.close();
        return true;
      }).catch(() => {
        Choerodon.prompt('引用失败');
        return false;
      });
    }
    return false;
  }, [issueTypeDataSet, linkDataSet, modal, renameExist]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={linkDataSet}>
      <SelectLinkType name="id" dataRef={linkedTypeRef} />
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
              该问题类型名称和当前项目已有的问题类型名称重复
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
    </Form>
  );
};

const ObserverLinkType = observer(LinkType);

const openLink = (props: Props) => {
  Modal.open({
    drawer: true,
    style: {
      width: 480,
    },
    key: 'link',
    title: '引用问题类型',
    children: <ObserverLinkType {...props} />,
  });
};

export default openLink;
