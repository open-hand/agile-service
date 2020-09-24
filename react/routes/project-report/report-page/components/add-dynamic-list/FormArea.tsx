import React, {
  ReactElement, useState, useCallback, useImperativeHandle,
} from 'react';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { Choerodon } from '@choerodon/boot';
import { Divider } from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import IssueFilterForm from '@/components/issue-filter-form';
import ChooseField from '@/components/chose-field';
import TableColumnCheckBoxes from '@/components/table-column-check-boxes';
import { useExportIssueStore } from './stores';
import { getCustomFieldFilters } from './utils';
import { ListRefProps } from '.';
import { SearchVO } from '../../store';
import './FormArea.less';

interface FormPartProps {
  title: string | ReactElement,
  children: ReactElement | ReactElement[] | null,
  btnOnClick?: (nextBtnStatusCode: 'ALL' | 'NONE') => boolean,
}
const FormPart: React.FC<FormPartProps> = (props) => {
  const {
    title, children, btnOnClick,
  } = props;
  const { prefixCls } = useExportIssueStore();
  const [btnStatus, setBtnStatus] = useState<'ALL' | 'NONE'>();
  function handleClick() {
    let result = true;
    const nextBtnStatus = btnStatus !== 'NONE' ? 'NONE' : 'ALL';
    if (typeof (btnOnClick) === 'function') {
      result = btnOnClick(nextBtnStatus);
    }
    result && setBtnStatus(nextBtnStatus);
  }
  return (
    <div className={`${prefixCls}-form`}>
      <div className={`${prefixCls}-form-title`}>
        <span>{title}</span>
        {!!btnOnClick && (
          <Button
            className={`${prefixCls}-form-btn`}
            color={'blue' as ButtonColor}
            onClick={handleClick}
          >
            {btnStatus !== 'NONE' ? '全选' : '全不选'}
          </Button>
        )}
      </div>
      {children}
    </div>
  );
};
interface Props {
  innerRef: React.MutableRefObject<ListRefProps>
}
const ExportIssue: React.FC<Props> = ({ innerRef }) => {
  const {
    prefixCls, checkOptions, choseFieldStore,
    tableColumnCheckBoxesDataSet, issueFilterFormDataSet,
  } = useExportIssueStore();
  const handleSubmit: ListRefProps['submit'] = useCallback(async () => {
    if (await issueFilterFormDataSet.current?.validate()) {
      const searchVO = getCustomFieldFilters(choseFieldStore.getAllChosenField, issueFilterFormDataSet.current!) as SearchVO;
      const colList = toJS(tableColumnCheckBoxesDataSet.current?.get('exportFieldCodes'));
      if (colList.length === 0) {
        Choerodon.prompt('至少选择1个字段');
        return false;
      }
      if (colList.length > 6) {
        Choerodon.prompt('最多可选6个字段');
        return false;
      }
      return {
        searchVO,
        colList,
      };
    }
    return false;
  }, [choseFieldStore.getAllChosenField, issueFilterFormDataSet, tableColumnCheckBoxesDataSet]);
  useImperativeHandle(innerRef, () => ({
    submit: handleSubmit,
  }), [handleSubmit]);
  return (
    <div>
      <FormPart title="筛选问题">
        <IssueFilterForm
          dataSet={issueFilterFormDataSet}
          chosenFields={choseFieldStore.getAllChosenField}
          onDelete={(item) => choseFieldStore.delChosenFields(item.code)}
        >
          <ChooseField store={choseFieldStore} dropDownBtnProps={{ icon: 'add', style: { color: '#3f51b5' } }} />
        </IssueFilterForm>
      </FormPart>
      <Divider className={`${prefixCls}-horizontal`} />
      <FormPart title="选择字段">
        <TableColumnCheckBoxes options={checkOptions} dataSet={tableColumnCheckBoxesDataSet} name="exportFieldCodes" />
      </FormPart>
    </div>
  );
};

export default observer(ExportIssue);
