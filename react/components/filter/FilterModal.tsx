import React, {
  useEffect, useRef, useCallback,
} from 'react';
import { Modal } from 'choerodon-ui/pro';
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import Filter, { useFilter } from '@/components/filter';
import { filterToSearchVO, ISearchVO, SearchVOToFilter } from './utils';

interface Props {
  modal?: IModalProps,
  searchVO?: ISearchVO
}
export interface RefProps {
  validate: () => Promise<boolean>
}

const FilterModal: React.FC<Props> = ({ modal, searchVO = {} }) => {
  const ref = useRef<RefProps>({} as RefProps);
  const initFilter = SearchVOToFilter(searchVO);
  const { state, handleSelectChange, handleFilterChange } = useFilter({
    filter: initFilter,
    selected: Object.keys(initFilter).filter((key) => initFilter[key] !== undefined),
  });
  const handleSubmit = useCallback(async () => {
    const validate = await ref.current.validate();
    if (validate) {
      console.log(state, filterToSearchVO(state.filter, [...state.systemFields, ...state.customFields]));
      return true;
    }
    return false;
  }, [state]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <>
      <Filter
        systemFields={state.systemFields}
        customFields={state.customFields}
        innerRef={ref}
        value={state.filter}
        selected={state.selected}
        onSelectChange={handleSelectChange}
        onFilterChange={handleFilterChange}
      />
    </>
  );
};

const openFilterModal = (props: Props) => {
  Modal.open({
    key: 'modal',
    title: '筛选',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <FilterModal {...props} />,
  });
};
export default openFilterModal;
