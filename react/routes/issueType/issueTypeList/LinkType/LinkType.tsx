import React, {
  useMemo, useEffect, useCallback, useRef, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Modal, DataSet, Form, Select, TextField,
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
  const [nameRepeat, setNameRepeat] = useState<boolean>(false);

  const checkName = useCallback(async (id) => {
    if (id) {
      const typeName = (linkedTypeRef.current || []).find((item) => item.id === id)?.name;
      if (typeName) {
        const res = await issueTypeApi.checkName(typeName);
        if (res) {
          setNameRepeat(true);
          return '该问题类型名称和当前项目已有的问题类型名称重复';
        }
        setNameRepeat(false);
        return true;
      }
      setNameRepeat(false);
      return true;
    }
    setNameRepeat(false);
    return true;
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
      dynamicProps: {
        required: async ({ record }) => {
          const typeIdsField = record.getField('id');
          const fieldValidate = await typeIdsField.checkValidity();
          if (record.get('id') && !fieldValidate) {
            return true;
          }
          return false;
        },
      },
    }],
  }), [checkName]);

  const handleSubmit = useCallback(async () => {
    let validate = false;
    if (nameRepeat) {
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
  }, [issueTypeDataSet, linkDataSet, modal, nameRepeat]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={linkDataSet}>
      <SelectLinkType name="id" dataRef={linkedTypeRef} />
      {
        nameRepeat && (
          <TextField name="newName" />
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
