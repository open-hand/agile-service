import React, {
  createContext, useCallback, useContext, useEffect, useMemo, useRef, useState,
} from 'react';
import { useCreation } from 'ahooks';
import {
  Modal, Form, DataSet, TextField, Select, SelectBox,
} from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { OldLoading as Loading } from '@/components/Loading';
import { IIssueType, IModalProps, IStatus } from '@/common/types';
import { getIsOrganization, getProjectId } from '@/utils/common';
import useIssueTypes from '@/hooks/data/useIssueTypes';
import useIsProgram from '@/hooks/useIsProgram';
import useDeepCompareEffect from '@/hooks/useDeepCompareEffect';
import {
  boardApiConfig, statusTransformApi, statusTransformApiConfig,
} from '@/api';
import useFormatMessage from '@/hooks/useFormatMessage';

export interface IStateMachineCreateStatusProps {
  selectedIssueType?: string[]
  onSubmit: Function
  modal?: IModalProps
  record?: Record
}
interface IStateMachineCreateStatusContext extends IStateMachineCreateStatusProps {
  isOrganization: boolean
  dataSet: DataSet
  setHasStatusIssueTypes: React.Dispatch<React.SetStateAction<IIssueType[]>>
  type:IStatus['valueCode'] | null
  issueTypes:any
  statusRecord?:Record
  setType: React.Dispatch<React.SetStateAction<IStatus['valueCode'] | null>>
}
const StateMachineCreateStatusContext = createContext({} as IStateMachineCreateStatusContext);
export function useStateMachineCreateStatusContext() {
  return useContext(StateMachineCreateStatusContext);
}
const StateMachineCreateStatusProvider: React.FC<IStateMachineCreateStatusProps> = (props) => {
  const {
    record: statusRecord, modal, selectedIssueType, onSubmit,
  } = props;
  const formatMessage = useFormatMessage();
  const [loading, setLoading] = useState<boolean>(false);
  const modalRef = useRef(modal);
  modalRef.current = modal;
  const isOrganization = getIsOrganization();
  const [type, setType] = useState<IStatus['valueCode'] | null>(null);
  const [editStatus, setEditStatus] = useState<any>(null);
  const { data: issueTypes } = useIssueTypes({ hasTemplate: true, excludeTypes: ['feature', 'issue_auto_test', 'issue_test', ...(isOrganization ? ['issue_epic'] : [])] });
  // 记录哪些类型下已经有同名状态
  const [hasStatusIssueTypes, setHasStatusIssueTypes] = useState<IIssueType[]>([]);
  const hasStatusIssueTypesRef = useRef<IIssueType[]>([]);
  hasStatusIssueTypesRef.current = hasStatusIssueTypes;
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    transport: {
      submit: ({ data: dataArray }) => {
        const data = dataArray[0];
        return statusRecord ? boardApiConfig.updateStatus(editStatus?.issueStatusId, {
          objectVersionNumber: editStatus?.issueStatusObjectVersionNumberId,
          completed: data.completed,
          statusId: editStatus?.id,
          id: editStatus?.issueStatusId,
          projectId: getProjectId(),
        }) : statusTransformApiConfig[isOrganization ? 'orgCreateStatus' : 'createStatus'](data.issueTypeIds, {
          name: data.name,
          type: data.valueCode,
          defaultStatus: data.default,
          transferAll: data.transferAll,
          completed: data.completed,
        });
      },
    },
    fields: [
      {
        name: 'name',
        type: 'string' as FieldType,
        label: '状态名称',
        required: true,
      },
      {
        name: 'valueCode',
        type: 'string' as FieldType,
        label: formatMessage({ id: 'agile.stateMachine.stage' }),
        required: true,
        lookupAxiosConfig: () => ({
          url: '/agile/v1/lookup_values/status_category',
          transformResponse: (data) => (Array.isArray(data) ? data : JSON.parse(data).lookupValues),
        }),
        textField: 'name',
        valueField: 'valueCode',
      },
      {
        name: 'issueTypeIds',
        type: 'string' as FieldType,
        label: formatMessage({ id: 'agile.common.issueType' }),
        required: true,
        textField: 'name',
        valueField: 'id',
        validator: async (value) => {
          const result = hasStatusIssueTypesRef.current.filter((item) => value && value.some((v: string) => v === item.id));
          if (result.length > 0) {
            return `${result.map((i) => i.name).join(',')}下已有同名状态`;
          }
          return true;
        },
      },
      {
        name: 'default',
        type: 'boolean' as FieldType,
        defaultValue: false,
        label: '是否设置为初始状态?',
        required: true,
      },
      {
        name: 'transferAll',
        type: 'boolean' as FieldType,
        defaultValue: true,
        label: '是否转换到所有状态?',
        required: true,
      },
      {
        name: 'completed',
        type: 'boolean' as FieldType,
        defaultValue: false,
        label: '是否为已解决状态？',
        required: true,
      },
    ],
  }), [editStatus?.id, editStatus?.issueStatusId, editStatus?.issueStatusObjectVersionNumberId, isOrganization, statusRecord]);
  useEffect(() => {
    if (selectedIssueType?.length) {
      dataSet.current?.set('issueTypeIds', selectedIssueType);
    }
  }, [dataSet, selectedIssueType]);
  useEffect(() => {
    if (type && type !== null) {
      dataSet.current?.set('categoryCode', type);
    }
  }, [dataSet, type]);
  const issueTypeIds = dataSet.current?.get('issueTypeIds');
  const disableCreate = useMemo(() => hasStatusIssueTypes.find((issueType) => (issueTypeIds || []).find((id: string) => id === issueType.id)), [hasStatusIssueTypes, issueTypeIds]);
  useDeepCompareEffect(() => {
    modalRef.current?.update({
      okProps: {
        disabled: !!disableCreate,
      },
    });
  }, [disableCreate]);
  useDeepCompareEffect(() => {
    if ((dataSet.current?.get('issueTypeIds') || []).length > 0) {
      dataSet.current?.getField('issueTypeIds')?.checkValidity();
    }
  }, [hasStatusIssueTypes]);
  const handleSubmit = useCallback(async () => {
    if (await dataSet.submit()) {
      onSubmit();
      return true;
    }
    return false;
  }, [dataSet, onSubmit]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  useEffect(() => {
    if (statusRecord) {
      setLoading(true);
      statusTransformApi.getStatus(statusRecord.get('id')).then((res: any) => {
        setEditStatus(res);
        setLoading(false);
        dataSet.current?.set('name', res.name);
        dataSet.current?.set('valueCode', res.type);
        dataSet.current?.set('issueTypeIds', res.issueTypeIds);
        dataSet.current?.set('completed', res.completed);
      });
    }
  }, [dataSet, statusRecord]);

  const value: IStateMachineCreateStatusContext = useCreation(() => ({
    ...props,
    isOrganization,
    dataSet,
    setType,
    issueTypes,
    type,
    statusRecord,
    setHasStatusIssueTypes,
  }), []);
  return (
    <StateMachineCreateStatusContext.Provider value={value}>
      <Loading loading={loading} />
      {props.children}
    </StateMachineCreateStatusContext.Provider>
  );
};
export default StateMachineCreateStatusProvider;
