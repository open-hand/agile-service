import { IChartSearchAdditionalProps } from '@/components/charts/types.';

export async function validateSearchDataBySearchProps(searchProps: IChartSearchAdditionalProps, searchData: any) {
  if (searchProps.searchDataSet) {
    return await searchProps.searchDataSet.validate() ? searchData : false;
  }
  return searchData;
}
