import React, {
  useMemo, useCallback, useEffect, useState, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Choerodon } from '@choerodon/boot';
import {
  DataSet, Form, Modal, TextField,
} from 'choerodon-ui/pro';
import { map } from 'lodash';
import { IModalProps, Issue, IIssueType } from '@/common/types';
import { issueApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import useIsInProgram from '@/hooks/useIsInProgram';
import SelectIssueType from '@/components/select/select-issue-type';
import EditIssueStore from '@/components/EditIssue/stores/EditIssueStore';
import openRequiredFieldsModal from '../../required-fields';
import styles from './TransformFromSubIssue.less';

interface Props {
  modal?: IModalProps
  issueId: string
  projectId?: string
  issueTypeId: string
  objectVersionNumber: number
  onOk: (issue: Issue) => void
  store: EditIssueStore
}
const TransformFromSubIssue: React.FC<Props> = ({
  modal, issueId, issueTypeId, objectVersionNumber, store, onOk, projectId,
}) => {
  const [isEpicType, setIsEpicType] = useState<boolean>(false);
  const issueTypesRef = useRef<IIssueType[]>([]);
  const { isInProgram, loading } = useIsInProgram();
  const transformFromSubIssueDs = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'typeId',
      label: '工作项类型',
      required: true,
    }, {
      name: 'epicName',
      label: '史诗名称',
      maxLength: 44,
      dynamicProps: {
        required: ({ record }) => {
          const epicTypes = (issueTypesRef.current || []).filter((item) => item.typeCode === 'issue_epic');
          const isEpic = !!map(epicTypes, 'id').find((id) => id === record.get('typeId')) || false;
          return isEpic;
        },
      },
    }],
    events: {
      update: async ({
        record, value, name,
      }: any) => {
        if (name === 'typeId' && value) {
          const epicTypes = (issueTypesRef.current || []).filter((item) => item.typeCode === 'issue_epic');
          const isEpic = !!map(epicTypes, 'id').find((id) => id === value) || false;
          setIsEpicType(isEpic);
          record.set('typeId', value);
          if (!isEpic && record.get('epicName')) {
            record.set('epicName', undefined);
          }
        }
      },
    },
  }), []);

  const handleSubmit = useCallback(async () => {
    const validate = await transformFromSubIssueDs.validate();
    const typeId = transformFromSubIssueDs.current?.get('typeId');
    if (validate) {
      const { typeCode } = (issueTypesRef.current || []).find((t) => t.id === typeId) || {};
      const issueUpdateTypeVO = {
        epicName: isEpicType ? transformFromSubIssueDs.current?.get('epicName') : undefined,
        issueId,
        objectVersionNumber,
        typeCode: typeCode as string,
        issueTypeId: typeId,
      };
      const issue = store.getIssue;
      const {
        // @ts-ignore
        summary, issueTypeVO = {},
      } = issue;
      const res = await issueApi.project(projectId).getRequiredField(issueId, typeId);
      const isNeedModal = res && res.filter((field: any) => field.fieldCode !== 'epicName').length > 0;
      if (isNeedModal) {
        modal?.close();
        openRequiredFieldsModal({
          projectId,
          requiredFields: res,
          issueVO: {
            summary,
            issueId,
            issueTypeVO,
            objectVersionNumber,
            typeCode: typeCode as string,
            issueTypeId: typeId,
          },
          reloadIssue: onOk,
          epicName: isEpicType ? transformFromSubIssueDs.current?.get('epicName') : undefined,
        });
      } else {
        return issueApi.project(projectId).updateType(issueUpdateTypeVO)
          .then((newIssue: Issue) => {
            onOk(newIssue);
            return true;
          }).catch(() => {
            Choerodon.prompt('转化失败');
            return false;
          });
      }
    }
    return false;
  }, [isEpicType, issueId, modal, objectVersionNumber, onOk, projectId, store.getIssue, transformFromSubIssueDs]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <div className={styles.transformFromSubIssue}>
      {
        loading ? null : (
          <Form dataSet={transformFromSubIssueDs}>
            <SelectIssueType
              name="typeId"
              projectId={projectId}
              filterList={isInProgram ? ['issue_epic', 'feature'] : ['feature']}
              dataRef={issueTypesRef}
              clearButton={false}
              excludeTypeIds={[issueTypeId]}
              onlyEnabled
            />
            {
              isEpicType && (
                <TextField name="epicName" />
              )
            }
          </Form>
        )
      }
    </div>
  );
};

const ObserverTransformFromSubIssue = observer(TransformFromSubIssue);

const openTransformFromSubIssue = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: '转化为工作项',
    children: <ObserverTransformFromSubIssue {...props} />,
    drawer: true,
    okText: '转化',
    style: {
      width: MODAL_WIDTH.small,
    },
  });
};

export default openTransformFromSubIssue;
