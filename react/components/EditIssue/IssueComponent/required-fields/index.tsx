import React, {
  useEffect, useCallback, useMemo, useState, useRef,
} from 'react';
import {
  Modal, Form, DataSet, Select,
} from 'choerodon-ui/pro';
import {
  includes, find,
} from 'lodash';
import { observer } from 'mobx-react-lite';
import { usePersistFn } from 'ahooks';
import Field from 'choerodon-ui/pro/lib/data-set/Field';
import useIsInProgram from '@/hooks/useIsInProgram';
import { IField, IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { OldLoading as Loading } from '@/components/Loading';
import { issueApi } from '@/api';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import RequiredField from '@/components/required-field';
import useRequiredFieldDataSet from '@/components/required-field/useRequiredFieldDataSet';
import styles from './index.less';

const { Option } = Select;
export interface ChangeTypeModalProps {
  modal?: IModalProps,
  requiredFields: IField[]
  issueVO: {
    issueTypeVO: {
      id: string
    }
    summary: string,
    issueId: string
    objectVersionNumber: number
    typeCode: string,
    issueTypeId: string,
  }
  reloadIssue: Function,
  onUpdate?: Function,
  epicName?: string
}

const extraFields = ['timeTrace'];

const ChangeTypeModal: React.FC<ChangeTypeModalProps> = (props) => {
  const { isInProgram } = useIsInProgram();
  let { data: issueTypeData = [] } = useProjectIssueTypes({ onlyEnabled: true });

  const {
    modal, requiredFields: fields, issueVO, reloadIssue, onUpdate,
  } = props;
  const [requiredFields, setRequiredFields] = useState(fields.filter((item) => !includes(extraFields, item.fieldCode)) || []);
  const [loading, setLoading] = useState<boolean>(false);

  const issueTypeIdDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'issueTypeId',
      label: '工作项类型',
      required: true,
      defaultValue: issueVO.issueTypeId,
    }],
    events: {
      update: async ({
        // @ts-ignore
        dataSet, name, value,
      }) => {
        if (name === 'issueTypeId') {
          if (resetDataRef.current) {
            resetDataRef.current(['issueTypeId']);
          }
          if (value) {
            setLoading(true);
            const res = await issueApi.getRequiredField(issueVO.issueId, value);
            setRequiredFields(res.filter((item: IField) => !includes(extraFields, item.fieldCode)));
            setLoading(false);
          } else {
            setRequiredFields([]);
          }
        }
      },
    },
  }), [issueVO.issueId, issueVO.issueTypeId]);

  const requiredFieldDsArr = useRequiredFieldDataSet([{
    issueId: issueVO.issueId,
    issueTypeId: issueTypeIdDataSet.current?.get('issueTypeId'),
    requiredFields: requiredFields || [],
  }]);

  const removeField = useCallback((name: string) => {
    requiredFieldDsArr[0]?.dataSet?.fields?.delete(name);
    requiredFieldDsArr[0]?.dataSet?.current?.fields.delete(name);
  }, [requiredFieldDsArr]);

  const resetDataRef = useRef<(excludeFieldsCode: string[]) => void>(() => { });
  const resetData = useCallback((excludeFieldsCode: string[]) => {
    const fieldNames: string[] = [];
    (requiredFieldDsArr[0]?.dataSet.current?.fields || []).forEach((field: Field) => {
      if (!includes(excludeFieldsCode, field.get('name'))) {
        field.reset();
        requiredFieldDsArr[0]?.dataSet.current?.set(field.get('name'), undefined);
        fieldNames.push(field.get('name'));
      }
    });
    fieldNames.forEach((name: string) => {
      removeField(name);
    });
  }, [removeField, requiredFieldDsArr]);

  resetDataRef.current = resetData;

  const handleSubmit = usePersistFn(async () => {
    const issueTypeIdValidate = await issueTypeIdDataSet.validate();
    const validate = requiredFieldDsArr[0]?.dataSet ? await requiredFieldDsArr[0]?.dataSet.current?.validate() : true;
    if (issueTypeIdValidate && validate) {
      const submitData = requiredFields.length ? {
        issueId: issueVO.issueId,
        objectVersionNumber: issueVO.objectVersionNumber,
        typeCode: find(issueTypeData, { id: issueTypeIdDataSet.current?.get('issueTypeId') })?.typeCode as string,
        issueTypeId: issueTypeIdDataSet.current?.get('issueTypeId') as string,
        batchUpdateFieldsValueVo: requiredFieldDsArr[0]?.getData(),
      } : {
        epicName: issueVO.typeCode === 'issue_epic' ? issueVO.summary : undefined,
        issueId: issueVO.issueId,
        objectVersionNumber: issueVO.objectVersionNumber,
        typeCode: find(issueTypeData, { id: issueTypeIdDataSet.current?.get('issueTypeId') })?.typeCode as string,
        issueTypeId: issueTypeIdDataSet.current?.get('issueTypeId') as string,
      };
      const res = await issueApi.updateType(submitData);
      if (reloadIssue) {
        reloadIssue(res);
      }
      if (onUpdate) {
        onUpdate(res);
      }
      return true;
    }
    return false;
  });

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  useEffect(() => {
    if (requiredFields.filter((item) => item.fieldCode === 'epicName')) {
      requiredFieldDsArr[0]?.dataSet.current?.set('epicName', props.epicName || issueVO.summary);
    }
  }, [issueVO.summary, props.epicName, requiredFieldDsArr, requiredFields]);

  const { stateMachineId } = find(issueTypeData, { id: issueVO.issueTypeVO?.id }) || {};
  issueTypeData = issueTypeData.filter((item) => item.stateMachineId !== stateMachineId).filter((item) => !['feature', ...(issueVO.typeCode === 'sub_task' ? [] : ['sub_task'])].includes(item.typeCode));
  if (isInProgram) {
    issueTypeData = issueTypeData.filter((item) => item.typeCode !== 'issue_epic');
  }

  return (
    <>
      <Loading loading={loading} />
      <Form className={styles.changeType} dataSet={issueTypeIdDataSet} style={{ marginLeft: -5 }}>
        <div className={styles.part_title}>
          类型修改为
        </div>
        <Select name="issueTypeId">
          {
            issueTypeData.map((type) => (
              <Option value={type.id} key={type.id}>{type.name}</Option>
            ))
          }
        </Select>
      </Form>
      {
        !loading && requiredFields.length > 0 && (
          <div className={styles.part_title} style={{ marginTop: 10, marginBottom: 13 }}>补全必输字段</div>
        )
      }
      {
        !!requiredFieldDsArr.length && (
          <RequiredField
            requiredFields={requiredFields}
            requiredFieldDataSet={requiredFieldDsArr[0]?.dataSet}
          />
        )
      }
    </>
  );
};

const ObserverChangeTypeModal = observer(ChangeTypeModal);

const openRequiredFieldsModal = (props: ChangeTypeModalProps) => {
  Modal.open({
    key: 'changeTypeModal',
    className: styles.changeTypeModal,
    title: '修改工作项类型',
    drawer: true,
    style: {
      width: MODAL_WIDTH.middle,
    },
    children: <ObserverChangeTypeModal {...props} />,
  });
};
export default openRequiredFieldsModal;
