import React, { useCallback, useEffect, useMemo } from 'react';
import {
  DataSet, DatePicker, Form, Icon, Modal, Select, SelectBox,
} from 'choerodon-ui/pro';
import { isEmpty } from 'lodash';
import { Observer } from 'mobx-react-lite';
import moment from 'moment';
import { versionApi } from '@/api';
import { IModalProps } from '@/common/types';
import LINK_URL from '@/constants/LINK_URL';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { getProjectId } from '@/utils/common';
import to from '@/utils/to';
import styles from './index.less';

interface PublishVersionModalProps {
  handleOk?: ((data: any) => void) | (() => Promise<any>)
}
interface PublishVersionModalWithEditProps extends PublishVersionModalProps {
  publishDetailData: any
  data: any
}
const { Option } = Select;
const PublishVersion: React.FC<{ modal?: IModalProps } & PublishVersionModalWithEditProps> = ({
  modal, handleOk, publishDetailData, data,
}) => {
  const hasUnSolvedIssue = !!publishDetailData.fixIssueCount;
  const disabledMoveNewVersion = useMemo(() => isEmpty(publishDetailData.versionNames), [publishDetailData.versionNames]);

  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: false,
    paging: false,
    data: [{ unsolvedIssue: 'ignore', targetVersionId: disabledMoveNewVersion ? undefined : publishDetailData.versionNames[0].versionId }],
    dataToJSON: 'normal' as any,
    fields: [
      {
        name: 'unsolvedIssue',
        label: '未解决的工作项',
        type: 'string' as any,
        required: publishDetailData.fixIssueCount,
      },
      {
        name: 'releaseDate', label: '发布日期', required: true, min: moment(data.startDate, 'YYYY-MM-DD'),
      },
      {
        name: 'targetVersionId',
        label: '移动到的版本',
        dynamicProps: ({ record }) => ({ required: record.get('unsolvedIssue') === 'newVersion' }),
      },

    ],
  }), [data.startDate, disabledMoveNewVersion, publishDetailData.fixIssueCount, publishDetailData.versionNames]);
  const handleSubmit = useCallback(async () => {
    if (!await ds.current?.validate()) {
      return false;
    }

    const values = ds.current?.toJSONData();
    const { releaseDate, targetVersionId, unsolvedIssue } = values;
    const submitData = {
      versionId: data.versionId,
      projectId: getProjectId(),
      releaseDate,
      targetVersionId: unsolvedIssue === 'newVersion' ? targetVersionId : undefined,
    };
    await versionApi.publish(submitData);
    const result = handleOk && await handleOk({ ...values });
    return typeof (result) !== 'undefined' ? result : true;
  }, [data.versionId, ds, handleOk]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  const handleGoIssue = () => {
    to(LINK_URL.workListIssue, {
      type: 'project',
      params: {
        paramType: 'version',
        paramId: data.versionId,
        paramName: `版本${data.name}中的工作项`,
      },
    }, { blank: true });
  };
  return (
    <div>
      {hasUnSolvedIssue ? (
        <div className={styles.prompt}>
          <Icon type="error" style={{ color: 'red' }} />
          <span
            className={styles.link}
            role="none"
            onClick={handleGoIssue}
          >
            {`这个版本还有 ${publishDetailData.fixIssueCount} 个没有解决的工作项。`}
          </span>
        </div>
      ) : ''}
      <Observer>
        {() => (
          <Form dataSet={ds}>
            {hasUnSolvedIssue ? (
              <SelectBox name="unsolvedIssue" className={styles.selectBox}>
                <SelectBox.Option value="ignore">忽略并继续发布</SelectBox.Option>
                <SelectBox.Option value="newVersion" disabled={disabledMoveNewVersion}>移动工作项到版本</SelectBox.Option>
              </SelectBox>
            ) : null}
            {hasUnSolvedIssue && !disabledMoveNewVersion && ds.current?.get('unsolvedIssue') === 'newVersion' ? (
              <Select name="targetVersionId" placeholder="选择要移动到的版本">
                {publishDetailData.versionNames.map((i: any) => <Option value={i.versionId}>{i.name}</Option>)}
              </Select>
            ) : null}
            <DatePicker name="releaseDate" />
          </Form>
        )}
      </Observer>
    </div>
  );
};
function openPublishReleaseVersionModal(props: PublishVersionModalWithEditProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '发布版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <PublishVersion {...props} />,

  });
}

export default openPublishReleaseVersionModal;
