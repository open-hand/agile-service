import React, {
  useEffect, useMemo, useRef, useState,
} from 'react';
import { Modal, Form, Select } from 'choerodon-ui/pro';
import { C7NFormat } from '@choerodon/master';
import { isEmpty, omit } from 'lodash';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IModalProps, Issue } from '@/common/types';
import { fieldApi, issueApi } from '@/api';
import { uploadAttachment } from '@/utils/richText';
import localCacheStore from '@/stores/common/LocalCacheStore';
import BaseComponent, { CreateIssueBaseProps } from './BaseComponent';
import SelectProject from '@/components/select/select-project';
import { AGILE_TYPE_CODES } from '@/constants/TYPE_CODE';
import { ICategoryCode } from '@/hooks/useCategoryCodes';

export interface CreateIssueSelectProjectProps {
  showSelectProject?: boolean,
  /**
   * 查询的项目类别 仅在 `showSelectProject` 有效
   * @default 'N_AGILE'
   */
  queryProjectCategories?: ICategoryCode[]
}
export type CreateIssueBase0Props =CreateIssueBaseProps& CreateIssueSelectProjectProps;
export interface CreateIssueProps extends Omit<CreateIssueBase0Props, 'onSubmit'> {
  onCreate: (issue: Issue) => void,
  /** 创建来源 */
  originFrom?: 'scrumBoard' | 'Backlog'
  hiddenTypeCodes?: string[],
  applyType?: 'agile' | 'program' | 'waterfall' | 'risk',
  request?: (data: any, applyType?: 'agile' | 'program' | 'waterfall' | 'risk') => Promise<any>
  onCancel?: () => void,
}

export const CreateContent = (props: CreateIssueBase0Props) => {
  const {
    showSelectProject = false, projectId, modal, queryProjectCategories,
  } = props;
  const selectProjectRef = useRef<Select>();
  const category: ICategoryCode[] = useMemo(() => (!queryProjectCategories?.length ? ['N_AGILE'] : queryProjectCategories), [queryProjectCategories]);
  const [currentProjectId, setCurrentProjectId] = useState<string | never>();
  const handleProjectChange = (value: string) => {
    setCurrentProjectId(value);
  };
  useEffect(() => {
    if (showSelectProject) {
      // @ts-ignore
      modal?.handleOk(() => {
        if (!projectId) {
          selectProjectRef?.current?.validate();
        }
        return false;
      });
    }
  }, []);

  return (
    <>
      {showSelectProject && (
        <Form>
          <SelectProject
            ref={selectProjectRef}
            onChange={handleProjectChange}
            label="所属项目"
            required
            clearButton={false}
            category={category}
          />
        </Form>
      )}
      {(!showSelectProject || currentProjectId) && (
        <BaseComponent {...props} projectId={showSelectProject ? currentProjectId : projectId} />
      )}
    </>
  );
};

const openModal = (props: CreateIssueProps) => {
  const {
    projectId, applyType = 'agile', onCreate, onCancel, request, originFrom, title,
  } = props;
  let { isProgram } = props;
  const handleSubmit: CreateIssueBaseProps['onSubmit'] = async ({ data, fieldList, fileList }) => {
    const postApplyType = applyType === 'waterfall' && AGILE_TYPE_CODES.includes(data.typeCode) ? 'agile' : applyType;
    const res = request ? await request(data as any, postApplyType) : await issueApi.create(data as any, postApplyType);
    await fieldApi.createFieldValue(res.issueId, 'agile_issue', fieldList, data.projectId);
    if (fileList && fileList.length > 0) {
      if (fileList.some((one) => !one.url)) {
        await uploadAttachment(fileList, res.issueId, data.projectId);
      }
    }
    onCreate(res);
    localCacheStore.setItem('agile.issue.type.common.selected', data.issueTypeId);
  };
  const defaultIssueTypeId = isEmpty(props.defaultTypeId) ? localCacheStore.getItem('agile.issue.type.common.selected') : props.defaultTypeId;
  let issueTypeCode: string | string[] | undefined;
  if (originFrom && ['scrumBoard', 'Backlog'].includes(originFrom)) {
    issueTypeCode = ['bug', 'task', 'story', 'sub_task'];
    isProgram = props.isProgram ?? false;
  }
  Modal.open({
    drawer: true,
    style: {
      width: MODAL_WIDTH.middle,
    },
    bodyStyle: {
      overflowX: 'hidden',
    },
    key: 'create-issue',
    title: title || (
      <C7NFormat
        intlPrefix="agile.common"
        id="create.issue"
      />
    ),
    okText: <C7NFormat
      intlPrefix="boot"
      id="create"
    />,
    onCancel: () => {
      if (onCancel) {
        onCancel();
      }
    },
    children: <CreateContent typeCode={issueTypeCode} onSubmit={handleSubmit} {...omit(props, 'onSubmit')} defaultTypeId={defaultIssueTypeId} isProgram={isProgram} applyType={applyType} />,
  });
};
export default openModal;
