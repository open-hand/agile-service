import { axios } from '@choerodon/boot';
import { getProjectId } from '@/utils/common';


interface IColumnMaxMin {
    boardId: number,
    columnId: number,
    maxNum: number,
    minNum: number,
    objectVersionNumber: number,
    projectId: number,
}
interface UpdateColumn {
    boardId: number,
    columnId: number,
    objectVersionNumber: number,
    projectId: number,
    name: string,
}
interface UColumnSequence {
    boardId: number,
    columnId: number,
    objectVersionNumber: number,
    projectId: number,
    sequence: number,
}
/**
 * 迭代看板 列
 * @author dzc
 */
class BoardColumnApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }

  /**
       * 配置看板中列的最大最小值更新
       * @param columnId 
       * @param data 
       */
  updateMaxMinNum(columnId: number, data: IColumnMaxMin) {
    return axios.post(`${this.prefix}/board_column/${columnId}/column_contraint`, data);
  }

  /**
     * 更新配置看板列
     * @param columnId 
     * @param boardId 
     * @param data 
     */
  update(columnId: number, boardId: number, data: UpdateColumn) {
    return axios({
      method: 'put',
      url: `${this.prefix}/board_column/${columnId}`,
      params: {
        boardId,
      },
      data,
    });
  }

  /**
     * 更新配置列顺序
     * @param data 
     */
  updateSequence(data: UColumnSequence) {
    return axios.post(`${this.prefix}/board_column/column_sort`, data);
  }

  /**
   * 删除配置列
   * @param columnId 
   */
  delete(columnId:number) {
    return axios.delete(`${this.prefix}/board_column/${columnId}`);
  }
}

const boardColumnApi = new BoardColumnApi();
export { boardColumnApi };
