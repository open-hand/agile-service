import {
  Button, Dropdown,
  ModalProvider,
} from 'choerodon-ui/pro';
import classNames from 'classnames';
import { flatten } from 'lodash';
import { runInAction } from 'mobx';

import React, {
  useState, useCallback, useEffect, useMemo,
} from 'react';
import { useIssueSearchStore } from '@/components/issue-search';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { CustomReportSearchProps } from '@/routes/ReportHost/custom-report/components/ChartSearch/ChartSearch';
import ChoseField, { useChoseField } from '@/components/chose-field';
import { IChosenFieldField } from '@/components/chose-field/types';
import useClickOut from '@/hooks/useClickOut';
import useFormatMessage from '@/hooks/useFormatMessage';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import useGetIssueSearchData from './useGetIssueSearchData';
import CustomSearchFields from './Search';
import IntlProviderAsync from '../../../components/intl-provider-async';

import './index.less';

const InjectCustomSearch: React.FC<CustomReportSearchProps> = ({ searchVO, setSearchVO, projectId }) => {
  const prefixCls = 'c7n-agile-inject-custom-search';
  const [fields, setFields] = useState([] as any[]);
  const formatMessage = useFormatMessage('agile.common');
  const issueSearchStore = useIssueSearchStore({
    projectId,
    transformFilter,
    defaultSearchVO: searchVO,
    // @ts-ignore
    getSystemFields: () => getSystemFields().filter((f) => !['contents'].includes(f.code)),
  });
  const handleChoseField = useCallback((data: IChosenFieldField | IChosenFieldField[], status: 'add' | 'del') => {
    runInAction(() => {
      const isSelect = status === 'add';
      const newChoseFields = flatten([data]);
      newChoseFields.forEach((item) => {
        issueSearchStore.handleChosenFieldChange(isSelect, item as any);
      });
    });
  }, [issueSearchStore]);
  const [{ store: chosenStore }, choseFiledProps] = useChoseField({
    fields,
    events: {
      choseField: handleChoseField,
    },
    dropDownProps: { getPopupContainer: (node) => node.parentNode as any },
  });

  useEffect(() => {
    issueSearchStore.initChosenFields();
    issueSearchStore.loadCustomFields().then((res) => {
      runInAction(() => {
        setFields(issueSearchStore.getAllFields.filter((item: any) => !item.noDisplay && !item.defaultShow));
      });
    });
  }, [issueSearchStore]);

  const [hidden, setHidden] = useState(true);
  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const containerRef = useClickOut(handleClickOut);

  const { onChange, onClear } = useGetIssueSearchData({ store: issueSearchStore, chosenStore });
  const handleChange = useCallback(({ code }: any, value: any) => {
    onChange(code, value);
  }, [onChange]);
  const handleQuery = () => {
    const newSearchVO = issueSearchStore.getCustomFieldFilters();
    setSearchVO(newSearchVO);
  };
  const handleClear = () => {
    onClear();
    setSearchVO(undefined as unknown as any);
  };

  const isDoneSearch = useMemo(() => issueSearchStore.isHasFilter && searchVO, [issueSearchStore.isHasFilter, searchVO]);
  return (
    <div className={prefixCls}>
      <Dropdown
        hidden={hidden}
        overlay={(
          <div
            ref={containerRef as any}
            role="none"
            onMouseDown={(e) => e.stopPropagation()}
            className={`${prefixCls}-menu`}
          >
            <CustomSearchFields className={`${prefixCls}-menu-top`} fields={issueSearchStore.chosenFields} onChange={handleChange} getPopupContainer={() => containerRef.current} />
            <div className={`${prefixCls}-menu-bottom`}>
              <Button
                color={'primary' as any}
                onClick={() => {
                  handleQuery();
                  // handleChange()
                  setHidden(true);
                }}
                style={{ marginLeft: '.1rem' }}
              >
                查询
              </Button>
              <Button onClick={handleClear}>{formatMessage({ id: 'reset' })}</Button>
              <ChoseField
                {...choseFiledProps}
              />
            </div>
          </div>
        )}
        trigger={['click'] as any}
      >

        <Button
          icon="filter2"
          className={classNames(`${prefixCls}-search-btn`, { [`${prefixCls}-search-btn-active`]: isDoneSearch })}
          onClick={(e) => {
            e.nativeEvent.stopImmediatePropagation();
            setHidden((old) => !old);
          }}
        />
      </Dropdown>
    </div>
  );
};
const IntlInjectCustomSearch: React.FC<CustomReportSearchProps> = (props) => (
  <IntlProviderAsync>
    <InjectCustomSearch {...props} />
  </IntlProviderAsync>
);
export default IntlInjectCustomSearch;
